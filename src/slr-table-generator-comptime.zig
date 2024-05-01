const std = @import("std");
const debug = std.debug;

const stdout = std.io.getStdOut().writer();

const lex = @import("wff-lexing.zig");

pub const TableGeneratorError = error{
    shiftShiftError,
    shiftReduceError,
    shiftAcceptError,
    reduceReduceError,
    reduceAcceptError,
};

/// Remove leading and trailing whitespace from a string.
/// (Does not modify given string, just returns a slice of it)
fn strStrip(ascii_string: []u8) []u8 {
    var start: usize = 0;
    for (ascii_string, 0..) |c, i| {
        if (!std.ascii.isWhitespace(c)) {
            start = i;
            break;
        }
    }
    var end: usize = start;
    for (ascii_string[start..], start..) |c, i| {
        if (!std.ascii.isWhitespace(c)) {
            end = i + 1;
        }
    }
    return ascii_string[start..end];
}

const SymbolId = usize;

const Symbol = union(enum) {
    const Self = @This();

    id: SymbolId,

    fn eql(self: Self, other: Self) bool {
        return self.id == other.id;
    }
};

const Production = struct {
    // TODO: Both of these helper structs should probably have dynamically
    // allocated strings
    const Self = @This();

    lhs: SymbolId,
    rhs: []const SymbolId,

    fn eql(self: Self, other: Self) bool {
        if (!self.lhs.eql(other.lhs)) return false;
        if (self.rhs.len != other.rhs.len) return false;
        for (self.rhs, other.rhs) |sym1, sym2| {
            if (!sym1.eql(sym2)) return false;
        }

        return true;
    }
};

const ProductionInstance = struct {
    const Self = @This();

    production: Production,
    cursor: usize,

    fn fromProduction(production: Production) Self {
        return ProductionInstance{ .production = production, .cursor = 0 };
    }

    fn eql(self: Self, other: Self) bool {
        return self.cursor == other.cursor and self.production.eql(other.production);
    }

    fn readCursor(self: Self) ?Symbol {
        if (self.cursor >= self.production.rhs.len) {
            return null;
        } else {
            return self.production.rhs[self.cursor];
        }
    }

    fn copyAdvanceCursor(self: Self) Self {
        return ProductionInstance{ .production = self.production, .cursor = self.cursor + 1 };
    }
};

/// Verify that a type has an eql method so that it can be used in Grammar
/// generation.
/// (Copied and slightly modified from the standard libary's 
/// hash_map.zig::verifyContext function)
fn verifyGrammarSymbolType(comptime X: type) void {
    var allow_const_ptr = false;
    var allow_mutable_ptr = false;
    // Make sure X is a namespace type which may have member functions
    switch (@typeInfo(X)) {
        .Struct, .Union, .Enum => {},
        // Special-case .Opaque for a better error message
        .Opaque => @compileError("Grammar symbol must be a type with an eql member function.  Cannot use " ++ @typeName(X) ++ " because it is opaque.  Use a pointer instead."),
        .Pointer => |ptr| {
            if (ptr.size != .One) {
                @compileError("Grammar symbol must be a type with an eql member function.  Cannot use " ++ @typeName(X) ++ " because it is not a single pointer.");
            }
            X = ptr.child;
            allow_const_ptr = true;
            allow_mutable_ptr = !ptr.is_const;
            switch (@typeInfo(X)) {
                .Struct, .Union, .Enum, .Opaque => {},
                else => @compileError("Grammar symbol must be a type with an eql member function.  Cannot use " ++ @typeName(X)),
            }
        },
        else => @compileError("Grammar symbol must be a type with an eql member function.  Cannot use " ++ @typeName(X)),
    }

    // Keep track of multiple errors so we can report them all.
    var errors: []const u8 = "";

    // Put common errors here, they will only be evaluated
    // if the error is actually triggered.
    const lazy = struct {
        const prefix = "\n  ";
        const deep_prefix = prefix ++ "  ";
        const eql_signature = "fn (" ++ @typeName(X) ++ ", " ++
            @typeName(X) ++ ") bool";
        const err_invalid_eql_signature = prefix ++ @typeName(X) ++ ".eql must be " ++ eql_signature ++
            deep_prefix ++ "but is actually " ++ @typeName(@TypeOf(X.eql));
    };

    // Verify X.eql(self, X, X) => bool
    if (@hasDecl(X, "eql")) {
        const eql = X.eql;
        const info = @typeInfo(@TypeOf(eql));
        if (info == .Fn) {
            const func = info.Fn;
            const args_len = 2;
            if (func.params.len != args_len) {
                errors = errors ++ lazy.err_invalid_eql_signature;
            } else {
                var emitted_signature = false;
                inline for (0..args_len) |i| {
                    if (func.params[i].type) |Self| {
                        if (Self == X) {
                            // pass, this is always fine.
                        } else if (Self == *const X) {
                            if (!allow_const_ptr) {
                                if (!emitted_signature) {
                                    errors = errors ++ lazy.err_invalid_eql_signature;
                                    emitted_signature = true;
                                }
                                errors = errors ++ lazy.deep_prefix ++ std.fmt.comptimePrint("Parameter {d} must be ", .{i}) ++ @typeName(X) ++ ", but is " ++ @typeName(Self);
                                errors = errors ++ lazy.deep_prefix ++ "Note: Cannot be a pointer because it is passed by value.";
                            }
                        } else if (Self == *X) {
                            if (!allow_mutable_ptr) {
                                if (!emitted_signature) {
                                    errors = errors ++ lazy.err_invalid_eql_signature;
                                    emitted_signature = true;
                                }
                                if (!allow_const_ptr) {
                                    errors = errors ++ lazy.deep_prefix ++ std.fmt.comptimePrint("Parameter {d} must be ", .{i}) ++ @typeName(X) ++ ", but is " ++ @typeName(Self);
                                    errors = errors ++ lazy.deep_prefix ++ "Note: Cannot be a pointer because it is passed by value.";
                                } else {
                                    errors = errors ++ lazy.deep_prefix ++ std.fmt.comptimePrint("Parameter {d} must be ", .{i}) ++ @typeName(X) ++ " or " ++ @typeName(*const X) ++ ", but is " ++ @typeName(Self);
                                    errors = errors ++ lazy.deep_prefix ++ "Note: Cannot be non-const because it is passed by const pointer.";
                                }
                            }
                        } else {
                            if (!emitted_signature) {
                                errors = errors ++ lazy.err_invalid_eql_signature;
                                emitted_signature = true;
                            }
                            errors = errors ++ lazy.deep_prefix ++ std.fmt.comptimePrint("Parameter {d} must be ", .{i}) ++ @typeName(X);
                            if (allow_const_ptr) {
                                errors = errors ++ " or " ++ @typeName(*const X);
                                if (allow_mutable_ptr) {
                                    errors = errors ++ " or " ++ @typeName(*X);
                                }
                            }
                            errors = errors ++ ", but is " ++ @typeName(Self);
                        }
                    }
                }
                
                if (func.return_type.? != bool) {
                    if (!emitted_signature) {
                        errors = errors ++ lazy.err_invalid_eql_signature;
                        emitted_signature = true;
                    }
                    errors = errors ++ lazy.deep_prefix ++ "Return type must be bool, but was " ++ @typeName(func.return_type.?);
                }
                // If any of these are generic (null), we cannot verify them.
                // The call sites check the return type, but cannot check the
                // parameters.  This may cause compile errors with generic hash/eql functions.
            }
        } else {
            errors = errors ++ lazy.err_invalid_eql_signature;
        }
    } else {
        errors = errors ++ lazy.prefix ++ @typeName(X) ++ " must declare a eql function with signature " ++ lazy.eql_signature;
    }

    if (errors.len != 0) {
        // errors begins with a newline (from lazy.prefix)
        @compileError("Problems found with grammar symbol type " ++ @typeName(X) ++ ":" ++ errors);
    }
}


// NOTE: variables ids MUST START AT 0 and MUST BE SMALLER THAN ALL TERMINAL IDS and MUST BE ORDERED
// NOTE: comparison of symbols is not always done using .eql in below functions, should try to make this consistent
// NOTE: FOLLOW and FIRST sets generation code is way too nested, should ideally
//       be broken down into smaller functions
fn Grammar(comptime V: type, comptime T: type) type {
    // Verify both V and T have correct .eql methods
    verifyGrammarSymbolType(V);
    verifyGrammarSymbolType(T);

    return struct {
        const Self = @This();

        rules: []const Production,
        variables: []const V,
        terminals: []const T,

        fn initFromTuples(
            comptime rule_tuples: anytype,
            comptime start_variable: V,
        ) Self {
            comptime {
                // Initialize a grammar struct to populate.
                var grammar = Self {
                    .rules = &[_] Production {},
                    .variables = &[_] V {start_variable},
                    .terminals = &[_] T {},
                };
                
                // Throws a compiler error if the structure or types of the 
                // given tuples are invalid. 
                verifyTuples(rule_tuples);
                
                // Iterate over each rule tuple and populate our grammar rules.
                inline for (std.meta.fieldNames(@TypeOf(rule_tuples))) |rule_tuples_field_name| {
                    const rule_tuple = @field(rule_tuples, rule_tuples_field_name);
                    
                    // Initialize a rule struct to populate.
                    var rule = Production {
                        .lhs = undefined,
                        .rhs = &[_]SymbolId {},
                    };

                    // Unpack the left hand side of the rule. We have already 
                    // checked that it is of type V.
                    const lhs = rule_tuple.@"0";
                    if (grammar.getVariableId(lhs)) |id| {
                        rule.lhs = id;
                    } else {
                        // If we haven't seen this variable yet, append it to 
                        // the variables list.
                        rule.lhs = grammar.variables.len;
                        grammar.variables = grammar.variables ++ &[_]V {lhs};
                    }

                    // Iterate over the symbols on the right hand side of the 
                    // rule tuple and unpack each into the grammar rule.
                    const rhs = rule_tuple.@"1";
                    inline for (std.meta.fieldNames(@TypeOf(rhs))) |rhs_tuple_field_name| {
                        const symbol = @field(rhs, rhs_tuple_field_name);
                        if (@TypeOf(symbol) == V) {
                            if (grammar.getVariableId(symbol)) |id| {
                                rule.rhs = rule.rhs ++ &[_]SymbolId {id};
                            } else {
                                // If we haven't seen this variable yet, append
                                // it to the variables list.
                                rule.rhs = rule.rhs ++ &[_]SymbolId {grammar.variables.len};
                                grammar.variables = grammar.variables ++ &[_]V {symbol};
                            }
                        } else {
                            if (grammar.getTerminalId(symbol)) |id| {
                                rule.rhs = rule.rhs ++ &[_]SymbolId {id};
                            } else {
                                // If we haven't seen this terminal yet, append 
                                // it to the terminals list.
                                rule.rhs = rule.rhs ++ &[_]SymbolId {grammar.terminals.len};
                                grammar.terminals = grammar.terminals ++ &[_]T {symbol};
                            }
                        }
                    }
                    // Append the populated rule.
                    grammar.rules = grammar.rules ++ &[_]Production {rule};
                }
                return grammar;
            }
        }

        /// Verify the structure and types of the given tuples. Return void 
        /// since this will be done at comptime and all errors will be compiler
        /// errors.
        fn verifyTuples(comptime rule_tuples: anytype) void {
            const RuleTuplesType = @TypeOf(rule_tuples);
            const rule_tuples_type_info = @typeInfo(RuleTuplesType);

            // Verify rule_tuples is a tuple (struct with no named fields)
            if (rule_tuples_type_info != .Struct or !rule_tuples_type_info.Struct.is_tuple) {
                @compileError("Expected tuple of rules. Cannot use '" ++ @typeName(RuleTuplesType) ++ "'");
            }
            // Verify rule_tuples is not empty
            if (std.meta.fields(RuleTuplesType).len == 0) {
                @compileError( "rule_tuples cannot be empty");
            }

            // Verify each field in rule_tuples is a valid grammar rule
            inline for (std.meta.fieldNames(RuleTuplesType), 0..) |rule_tuples_field_name, i| {
                const rule = @field(rule_tuples, rule_tuples_field_name);
                const RuleType = @TypeOf(rule);
                const rule_type_info = @typeInfo(RuleType);

                // Each rule should be a tuple
                if (rule_type_info != .Struct or !rule_type_info.Struct.is_tuple) {
                    @compileError(std.fmt.comptimePrint("Each rule must be a tuple, found '" ++ @typeName(rule) ++ "' (rule {d})", .{i}));
                }
                // Each rule should have exactly 2 fields
                if (std.meta.fields(RuleType).len != 2) {
                    @compileError(std.fmt.comptimePrint("Each rule must have exactly 2 fields, found {d} fields (rule {d})", .{rule_type_info.fields.len, i}));
                }

                // The first field of each rule should be of type V
                const lhs = rule.@"0";
                if (@TypeOf(lhs) != V) {
                    @compileError(std.fmt.comptimePrint("The first field in each rule tuple must be of type '" ++ @typeName(V) ++ "', found '" ++ @typeName(@TypeOf(rule.@"0")) ++ "' (rule {d})", .{i}));
                }

                // The second field of each rule should be a tuple with fields 
                // of type V or T (the right hand side of a production).
                const rhs = rule.@"1";
                const RhsType = @TypeOf(rhs);
                const rhs_type_info = @typeInfo(RhsType);

                // Verify rhs is a tuple (struct with no named fields)
                if (rhs_type_info != .Struct or !rhs_type_info.Struct.is_tuple) {
                    @compileError(std.fmt.comptimePrint("The second field in each rule tuple must be a tuple, found '" ++ @typeName(RuleTuplesType) ++ "' (rule {d})", .{i}));
                }
                // Verify rhs is not empty
                if (std.meta.fields(RuleTuplesType).len == 0) {
                    @compileError( std.fmt.comptimePrint("The second field (a tuple) in each rule tuple cannot be empty (rule {d})", .{i}));
                }

                // Verify each symbol in rhs is of type T or V
                inline for (std.meta.fieldNames(RhsType), 0..) |rhs_field_name, j| {
                    const rhs_symbol = @field(rhs, rhs_field_name);
                    const RhsSymbolType = @TypeOf(rhs_symbol);
                    if (RhsSymbolType != V and RhsSymbolType != T) {
                        @compileError(std.fmt.comptimePrint("Expected field of type '" ++ @typeName(V) ++ "' or '" ++ @typeName(T) ++ "', found '" ++ @typeName(RhsSymbolType) ++ "' (rule {d}, RHS symbol {d})", .{i, j}));
                    }
                }
            }
        }
    
        fn getVariableId(self: Self, variable: V) ?usize {
            for (self.variables, 0..) |v, i| {
                if (variable.eql(v)) {
                    return i;
                }
            }
            return null;
        }

        fn getTerminalId(self: Self, terminal: T) ?usize {
            for (self.terminals, self.variables.len..) |t, i| {
                if (terminal.eql(t)) {
                    return i;
                }
            }
            return null;
        }
    };
}

const Token = enum {
    const Self = @This();

    Proposition,
    Not,
    LParen,
    RParen,
    And,
    Or,
    Cond,
    Bicond,

    fn eql(self: Self, other: Self) bool {
        return self == other;
    }
};

const Variable = struct {
    const Self = @This();

    name: []const u8,

    fn fromString(string: []const u8) Self {
        return Variable{.name = string};
    }

    fn eql(self: Self, other: Self) bool {
        return std.mem.eql(u8, self.name, other.name);
    }
};

test "dev" {
    comptime {
        const V = Variable.fromString;
        var g = Grammar(Variable, Token).initFromTuples(
            .{
                .{V("S"), .{V("wff")}},
                .{V("wff"), .{Token.Proposition}},
                .{V("wff"), .{Token.Not, V("wff")}},
                .{V("wff"), .{Token.LParen, V("wff"), Token.And, V("wff"), Token.RParen}},
                .{V("wff"), .{Token.LParen, V("wff"), Token.Or, V("wff"), Token.RParen}},
                .{V("wff"), .{Token.LParen, V("wff"), Token.Cond, V("wff"), Token.RParen}},
                .{V("wff"), .{Token.LParen, V("wff"), Token.Bicond, V("wff"), Token.RParen}},
            },
            V("S"),
        );
        _ = g;
    }
}

    //     fn getOffsetTerminalId(self: Self, terminal: Symbol) usize {
    //         return terminal.id - self.variables;
    //     }

    //     // NOTE: Assumes $ is last terminal symbol ID and S' is the first symbol ID
    //     //       (0). FOLLOW(S') will be initialized to {$}, which will then be
    //     //       propogated as needed
    //     fn getFollowSet(self: Self) ![][]const bool {
    //         var first_set = try self.getFirstSet();
    //         defer {
    //             for (first_set) |row| self.allocator.free(row);
    //             self.allocator.free(first_set);
    //         }

    //         var follow_set = try self.allocator.alloc([]bool, self.variables);
    //         errdefer self.allocator.free(follow_set);
    //         var num_follow_allocated: usize = 0;
    //         for (0..follow_set.len) |i| {
    //             follow_set[i] = try self.allocator.alloc(bool, self.terminals);
    //             for (0..follow_set[i].len) |j| {
    //                 follow_set[i][j] = false;
    //             }
    //             num_follow_allocated += 1;
    //         }
    //         // Initialize FOLLOW(S') to include END symbol.
    //         follow_set[0][self.terminals - 1] = true;
    //         errdefer for (follow_set[0..num_follow_allocated]) |list| {
    //             self.allocator.free(list);
    //         };

    //         for (0..self.variables) |v| {
    //             var seen = try self.allocator.alloc(bool, self.variables);
    //             defer self.allocator.free(seen);
    //             for (0..seen.len) |i| {
    //                 seen[i] = false;
    //             }
    //             seen[v] = true; // redundant?

    //             var stack = std.ArrayList(SymbolID).init(self.allocator);
    //             defer stack.deinit();
    //             try stack.append(@intCast(v));

    //             while (stack.popOrNull()) |top| {
    //                 for (self.rules) |rule| {
    //                     for (rule.rhs, 0..) |symbol, symbol_index| {
    //                         if (symbol.id != top) {
    //                             continue;
    //                         }
    //                         if (symbol_index + 1 == rule.rhs.len) {
    //                             if (!seen[rule.lhs.id]) { // optimize: if already caluclated, dont redo
    //                                 if (rule.lhs.id < top) {
    //                                     for (follow_set[rule.lhs.id], 0..) |isFollow, offset_follow_symbol_id| {
    //                                         if (isFollow) {
    //                                             follow_set[v][offset_follow_symbol_id] = true;
    //                                         }
    //                                     }
    //                                 } else {
    //                                     try stack.append(rule.lhs.id);
    //                                 }
    //                                 seen[rule.lhs.id] = true;
    //                             }
    //                         } else if (self.symbolIsTerminal(rule.rhs[symbol_index + 1])) {
    //                             follow_set[v][self.getOffsetTerminalId(rule.rhs[symbol_index + 1])] = true;
    //                         } else {
    //                             for (first_set[symbol.id], 0..) |isFirst, offset_first_symbol_id| {
    //                                 if (isFirst) {
    //                                     follow_set[v][offset_first_symbol_id] = true;
    //                                 }
    //                             }
    //                         }
    //                     }
    //                 }
    //             }
    //         }
    //         return follow_set;
    //     }

    //     // NOTE: $ (END character) can never be first
    //     fn getFirstSet(self: Self) ![][]bool {
    //         var firsts = try self.allocator.alloc([]bool, self.variables);
    //         errdefer self.allocator.free(firsts);
    //         var num_firsts_allocated: usize = 0;
    //         for (0..firsts.len) |i| {
    //             firsts[i] = try self.allocator.alloc(bool, self.terminals);
    //             for (0..firsts[i].len) |j| {
    //                 firsts[i][j] = false; // Set default state to false, including for $
    //             }
    //             num_firsts_allocated += 1;
    //         }
    //         errdefer for (firsts[0..num_firsts_allocated]) |list| {
    //             self.allocator.free(list);
    //         };

    //         // Iterate over all variable ID's
    //         for (0..self.variables) |v| {
    //             var seen = try self.allocator.alloc(bool, self.variables);
    //             defer self.allocator.free(seen);
    //             for (0..seen.len) |i| {
    //                 seen[i] = false;
    //             }
    //             seen[v] = true;

    //             var stack = std.ArrayList(SymbolID).init(self.allocator);
    //             defer stack.deinit();
    //             try stack.append(@intCast(v));

    //             while (stack.popOrNull()) |top| {
    //                 for (self.rules) |rule| {
    //                     const first_rule_symbol = rule.rhs[0];
    //                     if (rule.lhs.id == top) {
    //                         if (self.symbolIsTerminal(first_rule_symbol)) {
    //                             firsts[v][self.getOffsetTerminalId(first_rule_symbol)] = true;
    //                         } else if (!seen[first_rule_symbol.id]) {
    //                             if (first_rule_symbol.id < v) { // entry already populated
    //                                 for (firsts[first_rule_symbol.id], 0..) |isInFirstList, i| {
    //                                     if (isInFirstList) {
    //                                         firsts[v][i] = true;
    //                                     }
    //                                 }
    //                             } else {
    //                                 try stack.append(first_rule_symbol.id);
    //                             }
    //                             seen[first_rule_symbol.id] = true;
    //                         }
    //                     }
    //                 }
    //             }
    //         }

    //         return firsts;
    //     }

    //     fn getSymbolCount(self: Self) usize {
    //         return self.variables + self.terminals;
    //     }

    //     fn getRuleId(self: Self, rule: Production) ?usize {
    //         for (self.rules, 0..) |known_rule, i| {
    //             if (known_rule.eql(rule)) {
    //                 return i;
    //             }
    //         }
    //         return null;
    //     }

    //     fn symbolIsVariable(self: Self, sym: Symbol) bool {
    //         return sym.id < self.variables;
    //     }

    //     fn symbolIsTerminal(self: Self, sym: Symbol) bool {
    //         return self.variables <= sym.id and sym.id < self.getSymbolCount();
    //     }

    //     pub fn printProductionInstance(_: Self, prod: ProductionInstance) !void {
    //         try stdout.print("{d}", .{prod.production.lhs.id});
    //         try stdout.print(" ->", .{});
    //         for (prod.production.rhs[0..prod.cursor]) |sym| {
    //             try stdout.print(" {d}", .{sym.id});
    //         }
    //         try stdout.print(" *", .{});
    //         for (prod.production.rhs[prod.cursor..]) |sym| {
    //             try stdout.print(" {d}", .{sym.id});
    //         }
    //         try stdout.print("\n", .{});
    //     }
//     };
// }

// test "firsts_and_follows_simple" {
//     // R0: (0) -> (1) (augment)
//     // R1: (1) -> (2)
//     // R2: (1) -> 5
//     // R3: (1) -> (4)
//     // R4: (2) -> (3)
//     // R6: (3) -> (2)
//     // R7: (3) -> 6
//     // R8: (4) -> 7

//     // R5: (3) -> (1)
//     // $ = 8
//     var r0 = Production{ .lhs = Symbol{ .id = 0 }, .rhs = &[_]Symbol{Symbol{ .id = 1 }} };

//     var r1 = Production{
//         .lhs = Symbol{ .id = 1 },
//         .rhs = &[_]Symbol{Symbol{ .id = 2 }},
//     };

//     var r2 = Production{
//         .lhs = Symbol{ .id = 1 },
//         .rhs = &[_]Symbol{Symbol{ .id = 5 }},
//     };

//     var r3 = Production{
//         .lhs = Symbol{ .id = 1 },
//         .rhs = &[_]Symbol{Symbol{ .id = 4 }},
//     };

//     var r4 = Production{
//         .lhs = Symbol{ .id = 2 },
//         .rhs = &[_]Symbol{Symbol{ .id = 3 }},
//     };

//     // var r5 = Production{
//     //     .lhs = Symbol{.id = 2},
//     //     .rhs = &[_]Symbol{Symbol{.id = 0}},
//     //
//     // };

//     var r6 = Production{
//         .lhs = Symbol{ .id = 3 },
//         .rhs = &[_]Symbol{Symbol{ .id = 2 }},
//     };

//     var r7 = Production{
//         .lhs = Symbol{ .id = 3 },
//         .rhs = &[_]Symbol{Symbol{ .id = 6 }},
//     };

//     var r8 = Production{
//         .lhs = Symbol{ .id = 4 },
//         .rhs = &[_]Symbol{Symbol{ .id = 7 }},
//     };

//     var grammar = Grammar{
//         .allocator = std.testing.allocator,
//         .variables = 5,
//         .terminals = 4,
//         .rules = &[_]Production{ r0, r1, r2, r3, r4, r6, r7, r8 },
//     };

//     var first_set = try grammar.getFirstSet();
//     defer {
//         for (0..first_set.len) |i| {
//             grammar.allocator.free(first_set[i]);
//         }
//         grammar.allocator.free(first_set);
//     }

//     var expected_firsts = [_][4]bool{ .{ true, true, true, false }, .{ true, true, true, false }, .{ false, true, false, false }, .{ false, true, false, false }, .{ false, false, true, false } };
//     for (first_set, expected_firsts) |actual, expected| {
//         try std.testing.expectEqualSlices(bool, &expected, actual);
//     }

//     var follow = try grammar.getFollowSet();
//     defer {
//         for (0..follow.len) |i| {
//             grammar.allocator.free(follow[i]);
//         }
//         grammar.allocator.free(follow);
//     }

//     var expected_follow = [_][4]bool{ .{ false, false, false, true }, .{ false, false, false, true }, .{ false, false, false, true }, .{ false, false, false, true }, .{ false, false, false, true } };
//     for (follow, expected_follow) |actual, expected| {
//         try std.testing.expectEqualSlices(bool, &expected, actual);
//     }

//     // debug.print("\n", .{});
//     // for (follow, 0..) |list, i| {
//     //     debug.print("({d}):", .{i});
//     //     for (list, grammar.variables.len..) |isFollow, j| {
//     //         if (isFollow) {
//     //             debug.print(" {d}", .{j});
//     //         }
//     //     }
//     //     debug.print("\n", .{});
//     // }
// }

// test "firsts_and_follows_grammar1_0" {
//     // S' = 0
//     // wff = 1
//     // Proposition = 2
//     // Not = 3
//     // LParen = 4
//     // RParen = 5
//     // And = 6
//     // Or = 7
//     // Cond = 8
//     // Bicond = 9
//     // $ = 10
//     var r1 = Production{ .lhs = Symbol{ .id = 1 }, .rhs = &[_]Symbol{Symbol{ .id = 2 }} };

//     var r2 = Production{ .lhs = Symbol{ .id = 1 }, .rhs = &[_]Symbol{ Symbol{ .id = 3 }, Symbol{ .id = 1 } } };

//     var r3 = Production{ .lhs = Symbol{ .id = 1 }, .rhs = &[_]Symbol{
//         Symbol{ .id = 4 },
//         Symbol{ .id = 1 },
//         Symbol{ .id = 6 },
//         Symbol{ .id = 1 },
//         Symbol{ .id = 5 },
//     } };

//     var r4 = Production{ .lhs = Symbol{ .id = 1 }, .rhs = &[_]Symbol{
//         Symbol{ .id = 4 },
//         Symbol{ .id = 1 },
//         Symbol{ .id = 7 },
//         Symbol{ .id = 1 },
//         Symbol{ .id = 5 },
//     } };

//     var r5 = Production{ .lhs = Symbol{ .id = 1 }, .rhs = &[_]Symbol{
//         Symbol{ .id = 4 },
//         Symbol{ .id = 1 },
//         Symbol{ .id = 8 },
//         Symbol{ .id = 1 },
//         Symbol{ .id = 5 },
//     } };

//     var r6 = Production{ .lhs = Symbol{ .id = 0 }, .rhs = &[_]Symbol{
//         Symbol{ .id = 4 },
//         Symbol{ .id = 1 },
//         Symbol{ .id = 9 },
//         Symbol{ .id = 1 },
//         Symbol{ .id = 5 },
//     } };

//     var r0 = Production{ .lhs = Symbol{ .id = 0 }, .rhs = &[_]Symbol{Symbol{ .id = 1 }} };

//     var grammar = Grammar{
//         .allocator = std.testing.allocator,
//         .rules = &[_]Production{ r0, r1, r2, r3, r4, r5, r6 },
//         .variables = 2,
//         .terminals = 9,
//     };

//     var first_set = try grammar.getFirstSet();
//     defer {
//         for (0..first_set.len) |i| {
//             grammar.allocator.free(first_set[i]);
//         }
//         grammar.allocator.free(first_set);
//     }

//     var expected_firsts = [_][9]bool{
//         .{ true, true, true, false, false, false, false, false, false },
//         .{ true, true, true, false, false, false, false, false, false },
//     };
//     for (first_set, expected_firsts) |actual, expected| {
//         try std.testing.expectEqualSlices(bool, &expected, actual);
//     }

//     var follow = try grammar.getFollowSet();
//     defer {
//         for (0..follow.len) |i| {
//             grammar.allocator.free(follow[i]);
//         }
//         grammar.allocator.free(follow);
//     }

//     var expected_follow = [_][9]bool{
//         .{ false, false, false, false, false, false, false, false, true },
//         .{ false, false, false, true, true, true, true, true, true },
//     };
//     for (follow, expected_follow) |actual, expected| {
//         try std.testing.expectEqualSlices(bool, &expected, actual);
//     }

//     // debug.print("\n", .{});
//     // for (follow, 0..) |list, i| {
//     //     debug.print("({d}):", .{i});
//     //     for (list, grammar.variables.len..) |isFollow, j| {
//     //         if (isFollow) {
//     //             debug.print(" {d}", .{j});
//     //         }
//     //     }
//     //     debug.print("\n", .{});
//     // }
// }

// test "follows_grammar2_2" {
//     const V_S = 0;
//     const V_WFF1 = 1;
//     const V_WFF2 = 2;
//     const V_WFF3 = 3;
//     const V_WFF4 = 4;
//     const V_PROP = 5;
//     const T_BICOND = 6;
//     const T_COND = 7;
//     const T_OR = 8;
//     const T_AND = 9;
//     const T_NOT = 10;
//     const T_LPAREN = 11;
//     const T_RPAREN = 12;
//     const T_PROPTOK = 13;
//     // const T_DOLLAR   = 14;

//     var r1 = Production{ .lhs = Symbol{ .id = V_S }, .rhs = &[_]Symbol{Symbol{ .id = V_WFF1 }} };

//     var r2 = Production{ .lhs = Symbol{ .id = V_WFF1 }, .rhs = &[_]Symbol{
//         Symbol{ .id = V_WFF1 },
//         Symbol{ .id = T_BICOND },
//         Symbol{ .id = V_WFF2 },
//     } };

//     var r3 = Production{ .lhs = Symbol{ .id = V_WFF2 }, .rhs = &[_]Symbol{Symbol{ .id = V_WFF3 }} };

//     var r4 = Production{ .lhs = Symbol{ .id = V_WFF2 }, .rhs = &[_]Symbol{
//         Symbol{ .id = V_WFF2 },
//         Symbol{ .id = T_COND },
//         Symbol{ .id = V_WFF3 },
//     } };

//     var r5 = Production{ .lhs = Symbol{ .id = V_WFF3 }, .rhs = &[_]Symbol{Symbol{ .id = V_WFF4 }} };

//     var r6 = Production{ .lhs = Symbol{ .id = V_WFF1 }, .rhs = &[_]Symbol{
//         Symbol{ .id = V_WFF3 },
//         Symbol{ .id = T_OR },
//         Symbol{ .id = V_WFF4 },
//     } };

//     var r7 = Production{ .lhs = Symbol{ .id = V_WFF3 }, .rhs = &[_]Symbol{
//         Symbol{ .id = V_WFF3 },
//         Symbol{ .id = T_AND },
//         Symbol{ .id = V_WFF4 },
//     } };

//     var r8 = Production{ .lhs = Symbol{ .id = V_WFF4 }, .rhs = &[_]Symbol{Symbol{ .id = V_PROP }} };

//     var r9 = Production{ .lhs = Symbol{ .id = V_WFF4 }, .rhs = &[_]Symbol{
//         Symbol{ .id = T_NOT },
//         Symbol{ .id = V_WFF4 },
//     } };

//     var r10 = Production{ .lhs = Symbol{ .id = V_PROP }, .rhs = &[_]Symbol{
//         Symbol{ .id = T_LPAREN },
//         Symbol{ .id = V_WFF1 },
//         Symbol{ .id = T_RPAREN },
//     } };

//     var r11 = Production{ .lhs = Symbol{ .id = V_PROP }, .rhs = &[_]Symbol{
//         Symbol{ .id = T_PROPTOK },
//     } };

//     var r0 = Production{ .lhs = Symbol{ .id = V_S }, .rhs = &[_]Symbol{Symbol{ .id = V_WFF1 }} };

//     var grammar = Grammar{
//         .allocator = std.testing.allocator,
//         .rules = &[_]Production{ r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11 },
//         .variables = 6,
//         .terminals = 9,
//     };

//     var first_set = try grammar.getFirstSet();
//     defer {
//         for (0..first_set.len) |i| {
//             grammar.allocator.free(first_set[i]);
//         }
//         grammar.allocator.free(first_set);
//     }

//     var expected_firsts = [_][9]bool{
//         .{ false, false, false, false, true, true, false, true, false },
//         .{ false, false, false, false, true, true, false, true, false },
//         .{ false, false, false, false, true, true, false, true, false },
//         .{ false, false, false, false, true, true, false, true, false },
//         .{ false, false, false, false, true, true, false, true, false },
//         .{ false, false, false, false, false, true, false, true, false },
//     };
//     for (first_set, expected_firsts) |actual, expected| {
//         try std.testing.expectEqualSlices(bool, &expected, actual);
//     }

//     var follow = try grammar.getFollowSet();
//     defer {
//         for (0..follow.len) |i| {
//             grammar.allocator.free(follow[i]);
//         }
//         grammar.allocator.free(follow);
//     }

//     var expected_follow = [_][9]bool{
//         .{ false, false, false, false, false, false, false, false, true },
//         .{ true, false, false, false, false, false, true, false, true },
//         .{ true, true, false, false, false, false, true, false, true },
//         .{ true, true, true, true, false, false, true, false, true },
//         .{ true, true, true, true, false, false, true, false, true },
//         .{ true, true, true, true, false, false, true, false, true },
//     };
//     for (follow, expected_follow) |actual, expected| {
//         try std.testing.expectEqualSlices(bool, &expected, actual);
//     }

//     // debug.print("\n", .{});
//     // for (follow, 0..) |list, i| {
//     //     debug.print("({d}):", .{i});
//     //     for (list, grammar.variables.len..) |isFollow, j| {
//     //         if (isFollow) {
//     //             debug.print(" {d}", .{j});
//     //         }
//     //     }
//     //     debug.print("\n", .{});
//     // }
// }

// const ParseTable = struct {
//     const Self = @This();
//     const Action = union(enum) {
//         state: usize,
//         reduce: usize, // index of grammar.rules
//         invalid,
//         accept,
//     };

//     grammar: Grammar,
//     action_goto_table: [][]Action,

//     fn expandProductions(self: Self, allocator: std.mem.Allocator, productions: []const ProductionInstance) ![]std.ArrayList(ProductionInstance) {
//         // const?
//         var branches = try allocator.alloc(std.ArrayList(ProductionInstance), self.grammar.getSymbolCount());
//         errdefer allocator.free(branches);

//         for (branches, 0..) |_, i| {
//             branches[i] = std.ArrayList(ProductionInstance).init(allocator);
//         }
//         errdefer {
//             for (branches) |list| {
//                 list.deinit();
//             }
//             allocator.free(branches);
//         }

//         var stack = std.ArrayList(ProductionInstance).init(allocator);
//         defer stack.deinit();

//         try stack.appendSlice(productions);

//         // Expand all given ProductionInstances and track all of the symbols
//         // currently being read.
//         while (stack.popOrNull()) |prod| {
//             if (prod.readCursor()) |sym| {
//                 if (self.grammar.symbolIsVariable(sym)) {
//                     if (branches[sym.id].items.len == 0) {
//                         for (self.grammar.rules) |rule| {
//                             if (sym.eql(rule.lhs)) {
//                                 try stack.append(ProductionInstance.fromProduction(rule));
//                             }
//                         }
//                     }
//                     try branches[sym.id].append(prod.copyAdvanceCursor());
//                 } else {
//                     try branches[sym.id].append(prod.copyAdvanceCursor());
//                 }
//             }
//         }

//         return branches;
//     }

//     fn checkProductionsAlreadyExpanded(starting_productions_table: std.ArrayList([]ProductionInstance), productions: std.ArrayList(ProductionInstance)) ?usize {
//         for (starting_productions_table.items, 0..) |start_prod_list, state| {
//             for (productions.items) |prod| {
//                 for (start_prod_list) |start_prod| {
//                     if (prod.eql(start_prod)) break;
//                 } else {
//                     break;
//                 }
//             } else {
//                 return state;
//             }
//         }

//         return null;
//     }

//     fn populate(self: *Self, allocator: std.mem.Allocator, augmented_production: Production) !void {
//         // TODO: defers
//         var action_goto_table = std.ArrayList([]Action).init(allocator);
//         defer action_goto_table.deinit();
//         errdefer for (action_goto_table.items) |row| {
//             allocator.free(row);
//         };
//         var primary_productions_table = std.ArrayList([]ProductionInstance).init(allocator);
//         defer {
//             for (primary_productions_table.items) |row| {
//                 allocator.free(row);
//             }
//             primary_productions_table.deinit();
//         }

//         try action_goto_table.append(try allocator.alloc(Action, self.grammar.getSymbolCount()));
//         try primary_productions_table.append(try allocator.alloc(ProductionInstance, 1));
//         primary_productions_table.items[0][0] = ProductionInstance.fromProduction(augmented_production);

//         var state: usize = 0;
//         while (state < action_goto_table.items.len) : (state += 1) {
//             var branches = try self.expandProductions(allocator, primary_productions_table.items[state]);
//             defer {
//                 for (branches) |prod_list| {
//                     prod_list.deinit();
//                 }
//                 allocator.free(branches);
//             }
//             for (branches, 0..) |*prod_list, id| {
//                 if (prod_list.items.len == 0) {
//                     action_goto_table.items[state][id] = Action.invalid;
//                 } else if (checkProductionsAlreadyExpanded(primary_productions_table, prod_list.*)) |existing_state| {
//                     action_goto_table.items[state][id] = try switch (action_goto_table.items[state][id]) {
//                         .invalid => Action{ .state = existing_state },
//                         .state => TableGeneratorError.shiftShiftError,
//                         .reduce => TableGeneratorError.shiftReduceError,
//                         .accept => TableGeneratorError.shiftAcceptError,
//                     };
//                 } else {
//                     var new_prod_list = try allocator.alloc(Action, self.grammar.getSymbolCount());
//                     try action_goto_table.append(new_prod_list);
//                     try primary_productions_table.append(try prod_list.toOwnedSlice());
//                     action_goto_table.items[state][id] = Action{ .state = action_goto_table.items.len - 1 };
//                 }
//             }
//         }

//         var follow_set = try self.grammar.getFollowSet();
//         defer {
//             for (follow_set) |row| allocator.free(row);
//             allocator.free(follow_set);
//         }

//         // TODO: Populate reductions
//         // For each completed primary production in each state, identify the
//         // production rule's index and insert a reduction on each of the LHS's
//         // follow set
//         for (primary_productions_table.items, 0..) |row, state_num| {
//             for (row) |instance| {
//                 if (instance.readCursor() != null) continue;

//                 var rule_id = self.grammar.getRuleId(instance.production).?;
//                 for (follow_set[instance.production.lhs.id], self.grammar.variables..) |isFollow, terminal_id| {
//                     if (!isFollow) continue;

//                     action_goto_table.items[state_num][terminal_id] = try switch (action_goto_table.items[state_num][terminal_id]) {
//                         .invalid => Action{ .reduce = rule_id },
//                         .state => TableGeneratorError.shiftReduceError,
//                         .reduce => TableGeneratorError.reduceReduceError,
//                         .accept => TableGeneratorError.reduceAcceptError,
//                     };
//                 }

//                 // TODO: hardcoded 0 kinda yucky
//                 if (instance.production.lhs.id == 0) {
//                     switch (action_goto_table.items[state_num][self.grammar.terminals - 1]) {
//                         .invalid, .accept => action_goto_table.items[state_num][self.grammar.terminals - 1] = Action.accept,
//                         .state => return TableGeneratorError.shiftAcceptError,
//                         .reduce => return TableGeneratorError.reduceAcceptError,
//                     }
//                 }
//             }
//         }

//         self.action_goto_table = try action_goto_table.toOwnedSlice();
//     }
// };

// test "expandProductions-grammar1_0" {
//     // S' = 0
//     // wff = 1
//     // Proposition = 2
//     // Not = 3
//     // LParen = 4
//     // RParen = 5
//     // And = 6
//     // Or = 7
//     // Cond = 8
//     // Bicond = 9
//     // $ = 10

//     var r1 = Production{ .lhs = Symbol{ .id = 1 }, .rhs = &[_]Symbol{Symbol{ .id = 2 }} };

//     var r2 = Production{ .lhs = Symbol{ .id = 1 }, .rhs = &[_]Symbol{ Symbol{ .id = 3 }, Symbol{ .id = 1 } } };

//     var r3 = Production{ .lhs = Symbol{ .id = 1 }, .rhs = &[_]Symbol{
//         Symbol{ .id = 4 },
//         Symbol{ .id = 1 },
//         Symbol{ .id = 6 },
//         Symbol{ .id = 1 },
//         Symbol{ .id = 5 },
//     } };

//     var r4 = Production{ .lhs = Symbol{ .id = 1 }, .rhs = &[_]Symbol{
//         Symbol{ .id = 4 },
//         Symbol{ .id = 1 },
//         Symbol{ .id = 7 },
//         Symbol{ .id = 1 },
//         Symbol{ .id = 5 },
//     } };

//     var r5 = Production{ .lhs = Symbol{ .id = 1 }, .rhs = &[_]Symbol{
//         Symbol{ .id = 4 },
//         Symbol{ .id = 1 },
//         Symbol{ .id = 8 },
//         Symbol{ .id = 1 },
//         Symbol{ .id = 5 },
//     } };

//     var r6 = Production{ .lhs = Symbol{ .id = 0 }, .rhs = &[_]Symbol{
//         Symbol{ .id = 4 },
//         Symbol{ .id = 1 },
//         Symbol{ .id = 9 },
//         Symbol{ .id = 1 },
//         Symbol{ .id = 5 },
//     } };

//     var r0 = Production{ .lhs = Symbol{ .id = 0 }, .rhs = &[_]Symbol{Symbol{ .id = 1 }} };

//     var grammar = Grammar{
//         .allocator = std.testing.allocator,
//         .rules = &[_]Production{ r0, r1, r2, r3, r4, r5, r6 },
//         .variables = 2,
//         .terminals = 9,
//     };

//     var table = ParseTable{
//         .grammar = grammar,
//         .action_goto_table = undefined,
//     };

//     const start_productions = [_]ProductionInstance{ProductionInstance.fromProduction(r0)};
//     var branches = try table.expandProductions(std.testing.allocator, &start_productions);
//     defer {
//         for (branches) |list| {
//             list.deinit();
//         }
//         std.testing.allocator.free(branches);
//     }

//     for (branches, 0..) |prods, i| {
//         debug.print("\n{d}\n", .{i});
//         for (prods.items) |p| {
//             try grammar.printProductionInstance(p);
//         }
//     }

//     try table.populate(std.testing.allocator, r0);
//     defer {
//         for (table.action_goto_table) |row| {
//             std.testing.allocator.free(row);
//         }
//         std.testing.allocator.free(table.action_goto_table);
//     }

//     for (table.action_goto_table, 0..) |row, s| {
//         debug.print("{d: >3} ||", .{s});
//         for (row) |entry| switch (entry) {
//             .state => |state_num| debug.print(" {d: ^3} |", .{state_num}),
//             .reduce => |rule_num| debug.print("R{d: ^3} |", .{rule_num}),
//             .accept => debug.print(" ACC |", .{}),
//             .invalid => debug.print("     |", .{}),
//         };
//         debug.print("\n", .{});
//     }
// }
