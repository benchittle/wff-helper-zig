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
    acceptError,
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

//const SymbolIdx = i32;

const Symbol = union(enum) {
    const Self = @This();

    variable: u16,
    terminal: u16,

    fn eql(self: Self, other: Self) bool {
        return switch(self) {
            .variable => |idx1| switch(other) {
                .variable => |idx2| idx1 == idx2,
                .terminal => false,
            },
            .terminal => |idx1| switch(other) {
                .variable => false,
                .terminal => |idx2| idx1 == idx2,
            }
        };
    }
};

const Production = struct {
    // TODO: Both of these helper structs should probably have dynamically
    // allocated strings
    const Self = @This();

    lhs: Symbol,
    rhs: []const Symbol,

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
pub fn Grammar(comptime V: type, comptime T: type) type {
    // Verify both V and T have correct .eql methods
    verifyGrammarSymbolType(V);
    verifyGrammarSymbolType(T);

    return struct {
        const Self = @This();
        
        allocator: ?std.mem.Allocator, // TODO: Make this optional, remove initialized_at_comptime
        rules: []const Production,
        variables: []const V,
        terminals: []const T,

        pub fn initFromTuples(
            comptime rule_tuples: anytype,
            comptime start_variable: V,
            comptime end_token: T
        ) Self {
            comptime {
                // Initialize a grammar struct to populate.
                var grammar = Self {
                    .allocator = null,
                    .rules = &[_] Production {},
                    .variables = &[_] V {start_variable},
                    .terminals = &[_] T {},
                };
                
                // Throws a compiler error if the structure or types of the 
                // given tuples are invalid. 
                verifyTuples(rule_tuples);
                
                // Iterate over each rule tuple and populate our grammar rules.
                for (rule_tuples) |rule_tuple| {
                    // Initialize a rule struct to populate.
                    var rule = Production {
                        .lhs = undefined,
                        .rhs = &[_]Symbol {},
                    };

                    // Unpack the left hand side of the rule. We have already 
                    // checked that it is of type V.
                    const lhs = rule_tuple.@"0";
                    if (grammar.getVariableSymbol(lhs)) |id| {
                        rule.lhs = id;
                    } else {
                        // If we haven't seen this variable yet, append it to 
                        // the variables list.
                        rule.lhs = grammar.getVariableCount();
                        grammar.variables = grammar.variables ++ &[_]V {lhs};
                    }

                    // Iterate over the symbols on the right hand side of the 
                    // rule tuple and unpack each into the grammar rule.
                    const rhs = rule_tuple.@"1";
                    for (std.meta.fieldNames(@TypeOf(rhs))) |rhs_tuple_field_name| {
                        const var_or_token = @field(rhs, rhs_tuple_field_name);
                        if (@TypeOf(var_or_token) == V) {
                            if (grammar.getVariableSymbol(var_or_token)) |symbol| {
                                rule.rhs = rule.rhs ++ &[_]Symbol {symbol};
                            } else {
                                // If we haven't seen this variable yet, append
                                // it to the variables list.
                                rule.rhs = rule.rhs ++ &[_]Symbol {Symbol{.variable=grammar.getVariableCount()}};
                                grammar.variables = grammar.variables ++ &[_]V {var_or_token};
                            }
                        } else {
                            if (grammar.getTerminalSymbol(var_or_token)) |symbol| {
                                rule.rhs = rule.rhs ++ &[_]Symbol {symbol};
                            } else {
                                // If we haven't seen this terminal yet, append 
                                // it to the terminals list.
                                rule.rhs = rule.rhs ++ &[_]Symbol {Symbol{.terminal=grammar.getTerminalCount()}};
                                grammar.terminals = grammar.terminals ++ &[_]T {var_or_token};
                            }
                        }
                    }
                    // Append the populated rule.
                    grammar.rules = grammar.rules ++ &[_]Production {rule};
                }
                grammar.terminals = grammar.terminals ++ &[_]T {end_token};
                return grammar;
            }
        }
        
        pub fn deinit(self: Self) void {
            if (self.allocator) |allocator| {
                allocator.free(self.rules);
                allocator.free(self.variables);
                allocator.free(self.terminals);
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

        fn getRuleIdx(self: Self, rule: Production) ?usize {
            for (self.rules, 0..) |known_rule, i| {
                if (known_rule.eql(rule)) {
                    return i;
                }
            }
            return null;
        }

        fn getVariableCount(self: Self) usize {
            return self.variables.len;
        }

        fn getTerminalCount(self: Self) usize {
            return self.terminals.len;
        }

        fn getSymbolCount(self: Self) usize {
            return self.getVariableCount() + self.getTerminalCount();
        }

        fn getVariableSymbol(self: Self, variable: V) ?Symbol {
            for (self.variables, 0..) |v, variable_idx| {
                if (variable.eql(v)) {
                    return Symbol{.variable=variable_idx};
                }
            }
            return null;
        }

        fn getTerminalSymbol(self: Self, terminal: T) ?Symbol {
            for (self.terminals, 0..) |t, terminal_idx| {
                if (terminal.eql(t)) {
                    return Symbol{.terminal=terminal_idx};
                }
            }
            return null;
        }

        // fn getOffsetIdx(self: Self, idx: @TypeOf(Symbol.terminal)) @TypeOf(Symbol.terminal) {
        //     return self.getVariableCount() + idx;
        // }

        fn getStartVariableIdx(_: Self) std.meta.TagPayload(Symbol, Symbol.variable) {
            return 0;
        }

        fn getEndTerminalIdx(self: Self) std.meta.TagPayload(Symbol, Symbol.terminal) {
            return @intCast(self.getTerminalCount() - 1);
        }

        fn getStartRuleIdx(_: Self) usize {
            return 0;
        }
    
        // NOTE: $ (END character) can never be first
        fn getFirstSet(self: Self, allocator: std.mem.Allocator) ![][]bool {
            var firsts = try allocator.alloc([]bool, self.getVariableCount());
            errdefer allocator.free(firsts);
            var num_firsts_allocated: usize = 0;
            for (0..firsts.len) |i| {
                firsts[i] = try allocator.alloc(bool, self.getTerminalCount());
                for (0..firsts[i].len) |j| {
                    firsts[i][j] = false; // Set default state to false, including for $
                }
                num_firsts_allocated += 1;
            }
            errdefer for (firsts[0..num_firsts_allocated]) |list| {
                allocator.free(list);
            };

            // Iterate over all variable ID's
            for (0..self.getVariableCount()) |v_idx| {
                var seen = try allocator.alloc(bool, self.getVariableCount());
                defer allocator.free(seen);
                for (0..seen.len) |i| {
                    seen[i] = false;
                }
                seen[v_idx] = true;

                var stack = try std.ArrayList(Symbol).initCapacity(allocator, self.getVariableCount());
                defer stack.deinit();
                stack.appendAssumeCapacity(Symbol{.variable=@intCast(v_idx)});

                while (stack.popOrNull()) |top_symbol| {
                    for (self.rules) |rule| {
                        if (!rule.lhs.eql(top_symbol)) {
                            continue;
                        }
                        const first_rule_symbol = rule.rhs[0];
                        switch(first_rule_symbol) {
                            .terminal => |idx| firsts[v_idx][idx] = true,
                            .variable => |idx| {
                                if (seen[idx]) {
                                    continue;
                                }
                                if (idx < v_idx) { // entry already populated
                                    // TODO: Make union function?
                                    for (firsts[idx], 0..) |isInFirstList, i| {
                                        if (isInFirstList) {
                                            firsts[v_idx][i] = true;
                                        }
                                    }
                                } else {
                                    stack.appendAssumeCapacity(first_rule_symbol);
                                }
                                seen[idx] = true;
                            }
                        }
                    }
                }
            }
            return firsts;
        }

        // NOTE: $ (END character) can never be first
        fn getFirstSetComptime(comptime self: Self) []const []const bool {
            return comptime ret: {
                var firsts = [_][self.getTerminalCount()]bool {
                    [_]bool {false} ** self.getTerminalCount()
                } ** self.getVariableCount();
                
                // Iterate over all variable ID's
                for (0..self.getVariableCount()) |v_idx| {
                    var seen = [_]bool {false} ** self.getVariableCount();
                    seen[v_idx] = true;

                    var stack: []const Symbol = &[_]Symbol {Symbol{.variable=@intCast(v_idx)}};

                    while (stack.len > 0) {
                        const top_symbol = stack[stack.len - 1];
                        stack = stack[0..stack.len - 1];

                        for (self.rules) |rule| {
                            if (!rule.lhs.eql(top_symbol)) {
                                continue;
                            }
                            const first_rule_symbol = rule.rhs[0];
                            switch(first_rule_symbol) {
                                .terminal => |idx| firsts[v_idx][idx] = true,
                                .variable => |idx| {
                                    if (seen[idx]) {
                                        continue;
                                    }
                                    if (idx < v_idx) { // entry already populated
                                        // TODO: Make union function?
                                        for (firsts[idx], 0..) |isInFirstList, i| {
                                            if (isInFirstList) {
                                                firsts[v_idx][i] = true;
                                            }
                                        }
                                    } else {
                                        stack = stack ++ &[_]Symbol {first_rule_symbol};
                                    }
                                    seen[idx] = true;
                                }
                            }
                        }
                    }
                }
                const firsts_const = firsts;
                var firsts_as_slices = [_][]const bool {undefined} ** firsts_const.len;
                for (0..firsts_const.len) |i| {
                    firsts_as_slices[i] = &firsts_const[i];
                }
                const firsts_as_slices_const = firsts_as_slices;
                break :ret &firsts_as_slices_const;
            };
        }

        // NOTE: Assumes $ is last terminal symbol ID and S' is the first symbol ID
        //       (0). FOLLOW(S') will be initialized to {$}, which will then be
        //       propogated as needed
        fn getFollowSet(self: Self, allocator: std.mem.Allocator) ![][]const bool {
            const first_set = try self.getFirstSet(allocator);
            defer {
                for (first_set) |row| allocator.free(row);
                allocator.free(first_set);
            }

            var follow_set = try allocator.alloc([]bool, self.getVariableCount());
            errdefer allocator.free(follow_set);
            var num_follow_allocated: usize = 0;
            for (0..follow_set.len) |i| {
                follow_set[i] = try allocator.alloc(bool, self.getTerminalCount());
                for (0..follow_set[i].len) |j| {
                    follow_set[i][j] = false;
                }
                num_follow_allocated += 1;
            }
            // Initialize FOLLOW(startsymbol) to include end of input symbol.
            errdefer for (follow_set[0..num_follow_allocated]) |list| {
                allocator.free(list);
            };
            follow_set[0][self.getTerminalCount() - 1] = true;

            for (0..self.getVariableCount()) |v_idx| {
                var seen = try allocator.alloc(bool, self.getVariableCount());
                defer allocator.free(seen);
                for (0..seen.len) |i| {
                    seen[i] = false;
                }
                seen[v_idx] = true; // redundant?

                var stack = try std.ArrayList(Symbol).initCapacity(allocator, self.getVariableCount());
                defer stack.deinit();
                stack.appendAssumeCapacity(Symbol{.variable=@intCast(v_idx)});

                while (stack.popOrNull()) |top_symbol| {
                    for (self.rules) |rule| {
                        for (rule.rhs[0..rule.rhs.len - 1], 0..) |rhs_symbol, i| {
                            if (!rhs_symbol.eql(top_symbol)) {
                                continue;
                            }
                                
                            switch(rule.rhs[i + 1]) {
                                .terminal => |idx| follow_set[v_idx][idx] = true,
                                // TODO: Make union function?
                                .variable => |idx| for (first_set[idx], 0..) |isFirst, terminal_idx| {
                                    if (isFirst) {
                                        follow_set[v_idx][terminal_idx] = true;
                                    }
                                }
                            }
                        }

                        // Handle the last RHS symbol separetly //

                        const last_rhs_symbol = rule.rhs[rule.rhs.len - 1];
                        if (!last_rhs_symbol.eql(top_symbol) or seen[rule.lhs.variable]) {
                            continue;
                        }
                        // Check if we already have the Follow set for this variable
                        // TODO: Make this more explicit?
                        if (rule.lhs.variable < v_idx) { 
                            for (follow_set[rule.lhs.variable], 0..) |isFollow, terminal_idx| {
                                if (isFollow) {
                                    follow_set[v_idx][terminal_idx] = true;
                                }
                            }
                        } else {
                            stack.appendAssumeCapacity(rule.lhs);
                        }
                        seen[rule.lhs.variable] = true;
                    }
                }
            }
            return follow_set;
        }

        fn getFollowSetComptime(comptime self: Self) []const []const bool {
            return comptime ret: { 
                const first_set = self.getFirstSetComptime();

                var follow_set = [_][self.getTerminalCount()]bool {
                    [_]bool {false} ** self.getTerminalCount()
                } ** self.getVariableCount();
                
                // Initialize FOLLOW(startsymbol) to include end of input symbol.
                follow_set[0][self.getTerminalCount() - 1] = true;

                for (0..self.getVariableCount()) |v_idx| {
                    var seen = [_]bool {false} ** self.getVariableCount();
                    seen[v_idx] = true; // redundant?

                    var stack: []const Symbol = &[_]Symbol {Symbol{.variable=@intCast(v_idx)}};

                    while (stack.len > 0) {
                        const top_symbol = stack[stack.len - 1];
                        stack = stack[0..stack.len - 1];

                        for (self.rules) |rule| {
                            for (rule.rhs[0..rule.rhs.len - 1], 0..) |rhs_symbol, i| {
                                if (!rhs_symbol.eql(top_symbol)) {
                                    continue;
                                }
                                    
                                switch(rule.rhs[i + 1]) {
                                    .terminal => |idx| follow_set[v_idx][idx] = true,
                                    // TODO: Make union function?
                                    .variable => |idx| for (first_set[idx], 0..) |isFirst, terminal_idx| {
                                        if (isFirst) {
                                            follow_set[v_idx][terminal_idx] = true;
                                        }
                                    }
                                }
                            }

                            // Handle the last RHS symbol separetly //

                            const last_rhs_symbol = rule.rhs[rule.rhs.len - 1];
                            if (!last_rhs_symbol.eql(top_symbol) or seen[rule.lhs.variable]) {
                                continue;
                            }
                            // Check if we already have the Follow set for this variable
                            // TODO: Make this more explicit?
                            if (rule.lhs.variable < v_idx) { 
                                for (follow_set[rule.lhs.variable], 0..) |isFollow, terminal_idx| {
                                    if (isFollow) {
                                        follow_set[v_idx][terminal_idx] = true;
                                    }
                                }
                            } else {
                                stack = stack ++ &[_]Symbol {rule.lhs};
                            }
                            seen[rule.lhs.variable] = true;
                        }
                    }
                }
                const follow_set_const = follow_set;
                var follow_set_as_slices = [_][]const bool {undefined} ** follow_set_const.len;
                for (0..follow_set_const.len) |i| {
                    follow_set_as_slices[i] = &follow_set_const[i];
                }
                const follow_set_as_slices_const = follow_set_as_slices;
                break :ret &follow_set_as_slices_const;
            };
        }

        fn printDebugProductionInstance(_: Self, prod: ProductionInstance) !void {
            debug.print("({d})", .{prod.production.lhs.variable});
            debug.print(" ->", .{});
            for (prod.production.rhs[0..prod.cursor]) |sym| {
                switch(sym) {
                    .variable => |idx| debug.print(" ({d})", .{idx}),
                    .terminal => |idx| debug.print(" {d}", .{idx}),
                }
            }
            debug.print(" *", .{});
            for (prod.production.rhs[prod.cursor..]) |sym| {
                switch(sym) {
                    .variable => |idx| debug.print(" ({d})", .{idx}),
                    .terminal => |idx| debug.print(" {d}", .{idx}),
                }
            }
            debug.print("\n", .{});
        }
    };
}

const TestToken = enum {
    const Self = @This();

    Proposition,
    Not,
    LParen,
    RParen,
    And,
    Or,
    Cond,
    Bicond,
    End,

    fn eql(self: Self, other: Self) bool {
        return self == other;
    }

    fn getString(self: Self) []const u8 {
       return switch (self) {
            .Proposition => "prop",
            .Not => "~",
            .LParen => "(",
            .RParen => ")",
            .And => "^",
            .Or => "v", 
            .Cond => "=>",
            .Bicond => "<=>",
            .End => "$",
        };
    } 
};

const TestVariable = struct {
    const Self = @This();

    name: []const u8,

    fn fromString(string: []const u8) Self {
        return TestVariable{.name = string};
    }

    fn getString(self: Self) []const u8 {
        return self.name;
    }

    fn eql(self: Self, other: Self) bool {
        return std.mem.eql(u8, self.name, other.name);
    }
};

test "Grammar.initFromTuples-grammar1_0" {
    // R0: S   -> wff
    // R1: wff -> Proposition
    // R2: wff -> Not    wff
    // R3: wff -> LParen wff And    wff RParen
    // R4: wff -> LParen wff Or     wff RParen
    // R5: wff -> LParen wff Cond   wff RParen
    // R6: wff -> LParen wff Bicond wff RParen
    //
    const V_S = 0;
    const V_WFF = 1;
    const T_PROPOSITION = 0;
    const T_NOT = 1;
    const T_LPAREN = 2;
    const T_AND = 3;
    const T_RPAREN = 4;
    const T_OR = 5;
    const T_COND = 6;
    const T_BICOND = 7;
    // $ = 10

    const V = TestVariable.fromString;
    const G = Grammar(TestVariable, TestToken);
    comptime var actual_grammar = G.initFromTuples(
        .{
            .{V("S"), .{V("wff")}},
            .{V("wff"), .{TestToken.Proposition}},
            .{V("wff"), .{TestToken.Not, V("wff")}},
            .{V("wff"), .{TestToken.LParen, V("wff"), TestToken.And, V("wff"), TestToken.RParen}},
            .{V("wff"), .{TestToken.LParen, V("wff"), TestToken.Or, V("wff"), TestToken.RParen}},
            .{V("wff"), .{TestToken.LParen, V("wff"), TestToken.Cond, V("wff"), TestToken.RParen}},
            .{V("wff"), .{TestToken.LParen, V("wff"), TestToken.Bicond, V("wff"), TestToken.RParen}},
        },
        V("S"),
        TestToken.End,
    );
    defer actual_grammar.deinit();

    const r0 = Production{.lhs = .{.variable=V_S},   .rhs = &[_]Symbol {.{.variable=V_WFF}}};
    const r1 = Production{.lhs = .{.variable=V_WFF}, .rhs = &[_]Symbol {.{.terminal=T_PROPOSITION}}};
    const r2 = Production{.lhs = .{.variable=V_WFF}, .rhs = &[_]Symbol {.{.terminal=T_NOT},    .{.variable=V_WFF}}};
    const r3 = Production{.lhs = .{.variable=V_WFF}, .rhs = &[_]Symbol {.{.terminal=T_LPAREN}, .{.variable=V_WFF}, .{.terminal=T_AND},    .{.variable=V_WFF}, .{.terminal=T_RPAREN}}};
    const r4 = Production{.lhs = .{.variable=V_WFF}, .rhs = &[_]Symbol {.{.terminal=T_LPAREN}, .{.variable=V_WFF}, .{.terminal=T_OR},     .{.variable=V_WFF}, .{.terminal=T_RPAREN}}};
    const r5 = Production{.lhs = .{.variable=V_WFF}, .rhs = &[_]Symbol {.{.terminal=T_LPAREN}, .{.variable=V_WFF}, .{.terminal=T_COND},   .{.variable=V_WFF}, .{.terminal=T_RPAREN}}};
    const r6 = Production{.lhs = .{.variable=V_WFF}, .rhs = &[_]Symbol {.{.terminal=T_LPAREN}, .{.variable=V_WFF}, .{.terminal=T_BICOND}, .{.variable=V_WFF}, .{.terminal=T_RPAREN}}};

    var expected_grammar = G {
        .allocator = null,
        .rules = &[_]Production{ r0, r1, r2, r3, r4, r5, r6 },
        .variables = &[_]TestVariable {V("S"), V("wff")},
        .terminals = &[_]TestToken {TestToken.Proposition, TestToken.Not, TestToken.LParen, TestToken.And, TestToken.RParen, TestToken.Or, TestToken.Cond, TestToken.Bicond, TestToken.End},
    };
    defer expected_grammar.deinit();

    try std.testing.expectEqualDeep(expected_grammar.rules, actual_grammar.rules);
    try std.testing.expectEqualDeep(expected_grammar.variables, actual_grammar.variables);
    try std.testing.expectEqualDeep(expected_grammar.terminals, actual_grammar.terminals);
}

test "Grammar.initFromTuples-grammar2_2" {    
    const V_S       = 0;
    const V_WFF1    = 1;
    const V_WFF2    = 2;
    const V_WFF3    = 3;
    const V_WFF4    = 4;
    const V_PROP    = 5;
    const T_BICOND  = 0;
    const T_COND    = 1;
    const T_OR      = 2;
    const T_AND     = 3;
    const T_NOT     = 4;
    const T_LPAREN  = 5;
    const T_RPAREN  = 6;
    const T_PROPTOK = 7;

    // R0:  S -> wff1
    // R1:  wff1 -> wff2
    // R2:  wff1 -> wff1 <=> wff2
    // R3:  wff2 -> wff3 
    // R4:  wff2 -> wff2 => wff3
    // R5:  wff3 -> wff4 
    // R6:  wff3 -> wff3 v wff4 
    // R7:  wff3 -> wff3 ^ wff4
    // R8:  wff4 -> prop
    // R9:  wff4 -> ~ wff4
    // R10: prop -> (wff1)
    // R11: prop -> PROPTOK

    const V = TestVariable.fromString;
    const G = Grammar(TestVariable, TestToken);

    comptime var actual_grammar = G.initFromTuples(
        .{
            .{V("S"), .{V("wff1")}},

            .{V("wff1"), .{V("wff2")}},
            .{V("wff1"), .{V("wff1"), TestToken.Bicond, V("wff2")}},

            .{V("wff2"), .{V("wff3")}},
            .{V("wff2"), .{V("wff2"), TestToken.Cond, V("wff3")}},
            
            .{V("wff3"), .{V("wff4")}},
            .{V("wff3"), .{V("wff3"), TestToken.Or, V("wff4")}},
            .{V("wff3"), .{V("wff3"), TestToken.And, V("wff4")}},

            .{V("wff4"), .{V("prop")}},
            .{V("wff4"), .{TestToken.Not, V("wff4")}},

            .{V("prop"), .{TestToken.LParen, V("wff1"), TestToken.RParen}},
            .{V("prop"), .{TestToken.Proposition}},
        },
        V("S"),
        TestToken.End,
    );
    defer actual_grammar.deinit();

    const r0  = Production{ .lhs = .{.variable=V_S},    .rhs = &[_]Symbol {.{.variable=V_WFF1}} };
    const r1  = Production{ .lhs = .{.variable=V_WFF1}, .rhs = &[_]Symbol {.{.variable=V_WFF2}} };
    const r2  = Production{ .lhs = .{.variable=V_WFF1}, .rhs = &[_]Symbol {.{.variable=V_WFF1},   .{.terminal=T_BICOND}, .{.variable=V_WFF2}}};
    const r3  = Production{ .lhs = .{.variable=V_WFF2}, .rhs = &[_]Symbol {.{.variable=V_WFF3}} };
    const r4  = Production{ .lhs = .{.variable=V_WFF2}, .rhs = &[_]Symbol {.{.variable=V_WFF2},   .{.terminal=T_COND},   .{.variable=V_WFF3}}};
    const r5  = Production{ .lhs = .{.variable=V_WFF3}, .rhs = &[_]Symbol {.{.variable=V_WFF4}} };
    const r6  = Production{ .lhs = .{.variable=V_WFF3}, .rhs = &[_]Symbol {.{.variable=V_WFF3},   .{.terminal=T_OR},     .{.variable=V_WFF4}}};
    const r7  = Production{ .lhs = .{.variable=V_WFF3}, .rhs = &[_]Symbol {.{.variable=V_WFF3},   .{.terminal=T_AND},    .{.variable=V_WFF4}}};
    const r8  = Production{ .lhs = .{.variable=V_WFF4}, .rhs = &[_]Symbol {.{.variable=V_PROP}} };
    const r9  = Production{ .lhs = .{.variable=V_WFF4}, .rhs = &[_]Symbol {.{.terminal=T_NOT},    .{.variable=V_WFF4}}};
    const r10 = Production{ .lhs = .{.variable=V_PROP}, .rhs = &[_]Symbol {.{.terminal=T_LPAREN}, .{.variable=V_WFF1},   .{.terminal=T_RPAREN}}};
    const r11 = Production{ .lhs = .{.variable=V_PROP}, .rhs = &[_]Symbol {.{.terminal=T_PROPTOK}}};

    var expected_grammar = G{
        .allocator = null,
        .rules = &[_]Production{ r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11 },
        .variables = &[_]TestVariable {V("S"), V("wff1"), V("wff2"), V("wff3"), V("wff4"), V("prop")},
        .terminals = &[_]TestToken {.Bicond, .Cond, .Or, .And, .Not, .LParen, .RParen, .Proposition, .End},
    };
    defer expected_grammar.deinit();

    try std.testing.expectEqualDeep(expected_grammar.rules, actual_grammar.rules);
    try std.testing.expectEqualDeep(expected_grammar.variables, actual_grammar.variables);
    try std.testing.expectEqualDeep(expected_grammar.terminals, actual_grammar.terminals);
}

test "firsts_and_follows-grammar1_0" {
    const V = TestVariable.fromString;
    comptime var grammar = Grammar(TestVariable, TestToken).initFromTuples(
        .{
            .{V("S"), .{V("wff")}},
            .{V("wff"), .{TestToken.Proposition}},
            .{V("wff"), .{TestToken.Not, V("wff")}},
            .{V("wff"), .{TestToken.LParen, V("wff"), TestToken.And, V("wff"), TestToken.RParen}},
            .{V("wff"), .{TestToken.LParen, V("wff"), TestToken.Or, V("wff"), TestToken.RParen}},
            .{V("wff"), .{TestToken.LParen, V("wff"), TestToken.Cond, V("wff"), TestToken.RParen}},
            .{V("wff"), .{TestToken.LParen, V("wff"), TestToken.Bicond, V("wff"), TestToken.RParen}},
        },
        V("S"),
        TestToken.End
    );
    defer grammar.deinit();

    var allocator = std.testing.allocator;

    const first_set = try grammar.getFirstSet(allocator);
    defer {
        for (0..first_set.len) |i| {
            allocator.free(first_set[i]);
        }
        allocator.free(first_set);
    }

    const first_set_comptime = grammar.getFirstSetComptime();

    const expected_firsts = [_][9]bool{
        .{ true, true, true, false, false, false, false, false, false },
        .{ true, true, true, false, false, false, false, false, false },
    };
    for (first_set, expected_firsts) |actual, expected| {
        try std.testing.expectEqualSlices(bool, &expected, actual);
    }
    for (first_set_comptime, expected_firsts) |actual, expected| {
        try std.testing.expectEqualSlices(bool, &expected, actual);
    }

    const follow = try grammar.getFollowSet(allocator);
    defer {
        for (0..follow.len) |i| {
            allocator.free(follow[i]);
        }
        allocator.free(follow);
    }

    const follow_comptime = grammar.getFollowSetComptime();

    const expected_follow = [_][9]bool{
        .{ false, false, false, false, false, false, false, false, true },
        .{ false, false, false, true, true, true, true, true, true },
    };
    for (follow, expected_follow) |actual, expected| {
        try std.testing.expectEqualSlices(bool, &expected, actual);
    }
    for (follow_comptime, expected_follow) |actual, expected| {
        try std.testing.expectEqualSlices(bool, &expected, actual);
    }

    // debug.print("\n", .{});
    // for (follow, 0..) |list, i| {
    //     debug.print("({d}):", .{i});
    //     for (list, grammar.variables.len..) |isFollow, j| {
    //         if (isFollow) {
    //             debug.print(" {d}", .{j});
    //         }
    //     }
    //     debug.print("\n", .{});
    // }
}

test "firsts_and_follows-grammar2_2" {
    // R0:  S -> wff1
    // R1:  wff1 -> wff2
    // R2:  wff1 -> wff1 <=> wff2
    // R3:  wff2 -> wff3 
    // R4:  wff2 -> wff2 => wff3
    // R5:  wff3 -> wff4 
    // R6:  wff3 -> wff3 v wff4 
    // R7:  wff3 -> wff3 ^ wff4
    // R8:  wff4 -> prop
    // R9:  wff4 -> ~ wff4
    // R10: prop -> (wff1)
    // R11: prop -> PROPTOK

    const V = TestVariable.fromString;
    const G = Grammar(TestVariable, TestToken);

    comptime var grammar = G.initFromTuples(
        .{
            .{V("S"), .{V("wff1")}},

            .{V("wff1"), .{V("wff2")}},
            .{V("wff1"), .{V("wff1"), TestToken.Bicond, V("wff2")}},

            .{V("wff2"), .{V("wff3")}},
            .{V("wff2"), .{V("wff2"), TestToken.Cond, V("wff3")}},
            
            .{V("wff3"), .{V("wff4")}},
            .{V("wff3"), .{V("wff3"), TestToken.Or, V("wff4")}},
            .{V("wff3"), .{V("wff3"), TestToken.And, V("wff4")}},

            .{V("wff4"), .{V("prop")}},
            .{V("wff4"), .{TestToken.Not, V("wff4")}},

            .{V("prop"), .{TestToken.LParen, V("wff1"), TestToken.RParen}},
            .{V("prop"), .{TestToken.Proposition}},
        },
        V("S"),
        TestToken.End,
    );
    defer grammar.deinit();

    var allocator = std.testing.allocator;

    const first_set = try grammar.getFirstSet(allocator);
    defer {
        for (0..first_set.len) |i| {
            allocator.free(first_set[i]);
        }
        allocator.free(first_set);
    }

    const first_set_comptime = grammar.getFirstSetComptime();

    const expected_firsts = [_][9]bool{
        .{ false, false, false, false, true, true, false, true, false },
        .{ false, false, false, false, true, true, false, true, false },
        .{ false, false, false, false, true, true, false, true, false },
        .{ false, false, false, false, true, true, false, true, false },
        .{ false, false, false, false, true, true, false, true, false },
        .{ false, false, false, false, false, true, false, true, false },
    };
    for (first_set, expected_firsts) |actual, expected| {
        try std.testing.expectEqualSlices(bool, &expected, actual);
    }
    for (first_set_comptime, expected_firsts) |actual, expected| {
        try std.testing.expectEqualSlices(bool, &expected, actual);
    }

    const follow = try grammar.getFollowSet(allocator);
    defer {
        for (0..follow.len) |i| {
            allocator.free(follow[i]);
        }
        allocator.free(follow);
    }
    const follow_comptime = grammar.getFollowSetComptime();

    const expected_follow = [_][9]bool{
        .{ false, false, false, false, false, false, false, false, true },
        .{ true, false, false, false, false, false, true, false, true },
        .{ true, true, false, false, false, false, true, false, true },
        .{ true, true, true, true, false, false, true, false, true },
        .{ true, true, true, true, false, false, true, false, true },
        .{ true, true, true, true, false, false, true, false, true },
    };
    for (follow, expected_follow) |actual, expected| {
        try std.testing.expectEqualSlices(bool, &expected, actual);
    }
    for (follow_comptime, expected_follow) |actual, expected| {
        try std.testing.expectEqualSlices(bool, &expected, actual);
    }

    // debug.print("\n", .{});
    // for (follow, 0..) |list, i| {
    //     debug.print("({d}):", .{i});
    //     for (list, grammar.variables.len..) |isFollow, j| {
    //         if (isFollow) {
    //             debug.print(" {d}", .{j});
    //         }
    //     }
    //     debug.print("\n", .{});
    // }
}

fn ParseTable(comptime V: type, comptime T: type) type {
    return struct {
        const Self = @This();
        const GrammarType = Grammar(V, T);
        const Action = union(enum) {
            state: usize,
            reduce: usize, // index of grammar.rules
            invalid,
            accept,
        };

        allocator: std.mem.Allocator,
        grammar: GrammarType,
        goto_table: [][]Action,
        action_table: [][]Action,

        fn init(allocator: std.mem.Allocator, grammar: GrammarType) !Self {
            const goto_table, const action_table = try generateTables(allocator, grammar);

            return Self{
                .allocator = allocator,
                .grammar = grammar,
                .goto_table = goto_table,
                .action_table = action_table,
            };
        }

        /// Note: Does NOT free the memory associated with the grammar
        fn deinit(self: Self) void {
            for (self.goto_table, self.action_table) |goto_row, action_row| {
                self.allocator.free(goto_row);
                self.allocator.free(action_row);
            }
            self.allocator.free(self.goto_table);
            self.allocator.free(self.action_table);
        }
        
        fn printDebugTable(self: Self) void {
            const COL_SPACE = "4";
            debug.print("\n{s: ^" ++ COL_SPACE ++ "} ||", .{""});
            for (self.grammar.terminals) |t| {
                debug.print(" {s: ^" ++ COL_SPACE ++ "} |", .{t.getString()});
            }
            debug.print("|", .{});
            for (self.grammar.variables) |v| {
                debug.print(" {s: ^" ++ COL_SPACE ++ "} |", .{v.getString()});
            }
            debug.print("|", .{});
            
            for (self.action_table, self.goto_table, 0..) |action_row, goto_row, s| {
                debug.print("\n{d: >" ++ COL_SPACE ++ "} ||", .{s});
                for (action_row) |entry| switch (entry) {
                    .state => |state_num| debug.print(" {d: ^" ++ COL_SPACE ++ "} |", .{state_num}),
                    .reduce => |rule_num| debug.print("R{d: ^" ++ COL_SPACE ++ "} |", .{rule_num}),
                    .accept => debug.print(" {s: ^" ++ COL_SPACE ++ "} |", .{"ACC"}),
                    .invalid => debug.print(" {s: ^" ++ COL_SPACE ++ "} |", .{""}),
                };
                debug.print("|", .{});
                for (goto_row) |entry| switch (entry) {
                    .state => |state_num| debug.print(" {d: ^" ++ COL_SPACE ++ "} |", .{state_num}),
                    .reduce => |rule_num| debug.print("R{d: ^" ++ COL_SPACE ++ "} |", .{rule_num}),
                    .accept => debug.print(" {s: ^" ++ COL_SPACE ++ "} |", .{"ACC"}),
                    .invalid => debug.print(" {s: ^" ++ COL_SPACE ++ "} |", .{""}),
                };
                debug.print("| {d: <" ++ COL_SPACE ++ "}", .{s});
            }
            debug.print("\n", .{});
        }

        fn expandProductions(
            allocator: std.mem.Allocator, 
            grammar: GrammarType,
            productions: []const ProductionInstance
        ) !struct{[]std.ArrayList(ProductionInstance), []std.ArrayList(ProductionInstance)} {
            // const?
            var variable_branches = try allocator.alloc(std.ArrayList(ProductionInstance), grammar.getVariableCount());
            errdefer allocator.free(variable_branches);

            for (variable_branches, 0..) |_, i| {
                variable_branches[i] = std.ArrayList(ProductionInstance).init(allocator);
            }
            errdefer {
                for (variable_branches) |list| {
                    list.deinit();
                }
                allocator.free(variable_branches);
            }

            var terminal_branches = try allocator.alloc(std.ArrayList(ProductionInstance), grammar.getTerminalCount());
            errdefer allocator.free(terminal_branches);

            for (terminal_branches, 0..) |_, i| {
                terminal_branches[i] = std.ArrayList(ProductionInstance).init(allocator);
            }
            errdefer {
                for (terminal_branches) |list| {
                    list.deinit();
                }
                allocator.free(terminal_branches);
            }

            var stack = std.ArrayList(ProductionInstance).init(allocator);
            defer stack.deinit();

            try stack.appendSlice(productions);

            // Expand all given ProductionInstances and track all of the symbols
            // currently being read.
            while (stack.popOrNull()) |prod| {
                if (prod.readCursor()) |sym| {
                    switch(sym) {
                        .variable => |idx| {
                            // If the variable has not been encountered yet,
                            // expand it by pushing any productions from it onto
                            // the stack.
                            if (variable_branches[idx].items.len == 0) {
                                for (grammar.rules) |rule| {
                                    if (sym.eql(rule.lhs)) {
                                        try stack.append(ProductionInstance.fromProduction(rule));
                                    }
                                }
                            }
                            // Add the variable as a branch.
                            try variable_branches[idx].append(prod.copyAdvanceCursor());
                        },
                        .terminal => |idx| try terminal_branches[idx].append(prod.copyAdvanceCursor()),
                    }
                }
            }

            return .{variable_branches, terminal_branches};
        }

        fn checkStateAlreadyExists(starting_productions_table: std.ArrayList([]ProductionInstance), productions: std.ArrayList(ProductionInstance)) ?usize {
            for (starting_productions_table.items, 0..) |start_prod_list, state| {
                for (productions.items) |prod| {
                    for (start_prod_list) |start_prod| {
                        if (prod.eql(start_prod)) break;
                    } else {
                        break;
                    }
                } else {
                    return state;
                }
            }

            return null;
        }

        fn generateTables(allocator: std.mem.Allocator, grammar: GrammarType) !struct{[][]Action, [][]Action} {
            var goto_table = std.ArrayList([]Action).init(allocator);
            defer goto_table.deinit();
            errdefer for (goto_table.items) |row| {
                allocator.free(row);
            };
            var action_table = std.ArrayList([]Action).init(allocator);
            defer action_table.deinit();
            errdefer for (action_table.items) |row| {
                allocator.free(row);
            };
            var primary_productions_table = std.ArrayList([]ProductionInstance).init(allocator);
            defer {
                for (primary_productions_table.items) |row| {
                    allocator.free(row);
                }
                primary_productions_table.deinit();
            }

            try goto_table.append(try allocator.alloc(Action, grammar.getVariableCount()));
            try action_table.append(try allocator.alloc(Action, grammar.getTerminalCount()));
            try primary_productions_table.append(try allocator.alloc(ProductionInstance, 1));
            primary_productions_table.items[0][0] = ProductionInstance.fromProduction(grammar.rules[grammar.getStartRuleIdx()]);

            var state: usize = 0;
            while (state < goto_table.items.len) : (state += 1) {
                const variable_branches, const terminal_branches = try expandProductions(allocator, grammar, primary_productions_table.items[state]);
                defer {
                    for (variable_branches) |variable_prod_list| {
                        variable_prod_list.deinit();
                    }
                    for (terminal_branches) |terminal_prod_list| {
                        terminal_prod_list.deinit();
                    }
                    allocator.free(variable_branches);
                    allocator.free(terminal_branches);
                }
                

                for (variable_branches, 0..) |*prod_list, v_idx| {
                    // If there are no transitions from this variable, mark 
                    // this cell as invalid.
                    if (prod_list.items.len == 0) {
                        goto_table.items[state][v_idx] = Action.invalid;
                    // If expanding these productions would result in a state 
                    // that already exists, 
                    } else if (checkStateAlreadyExists(primary_productions_table, prod_list.*)) |existing_state| {
                        goto_table.items[state][v_idx] = try switch (goto_table.items[state][v_idx]) {
                            .invalid => Action{ .state = existing_state },
                            .state => TableGeneratorError.shiftShiftError,
                            .reduce => TableGeneratorError.shiftReduceError,
                            .accept => TableGeneratorError.shiftAcceptError,
                        };
                    } else {
                        try goto_table.append(try allocator.alloc(Action, grammar.getVariableCount()));
                        try action_table.append(try allocator.alloc(Action, grammar.getTerminalCount()));
                        try primary_productions_table.append(try prod_list.toOwnedSlice());
                        goto_table.items[state][v_idx] = Action{ .state = goto_table.items.len - 1 };
                    }
                }
                for (terminal_branches, 0..) |*prod_list, t_idx| {
                    // If there are no transitions from this variable, mark 
                    // this cell as invalid.
                    if (prod_list.items.len == 0) {
                        action_table.items[state][t_idx] = Action.invalid;
                    // If expanding these productions would result in a state 
                    // that already exists, 
                    } else if (checkStateAlreadyExists(primary_productions_table, prod_list.*)) |existing_state| {
                        action_table.items[state][t_idx] = try switch (action_table.items[state][t_idx]) {
                            .invalid => Action{ .state = existing_state },
                            .state => TableGeneratorError.shiftShiftError,
                            .reduce => TableGeneratorError.shiftReduceError,
                            .accept => TableGeneratorError.shiftAcceptError,
                        };
                    } else {
                        try goto_table.append(try allocator.alloc(Action, grammar.getVariableCount()));
                        try action_table.append(try allocator.alloc(Action, grammar.getTerminalCount()));
                        try primary_productions_table.append(try prod_list.toOwnedSlice());
                        action_table.items[state][t_idx] = Action{ .state = action_table.items.len - 1 };
                    }
                }
            }

            const follow_set = try grammar.getFollowSet(allocator);
            defer {
                for (follow_set) |row| allocator.free(row);
                allocator.free(follow_set);
            }

            // TODO: Populate reductions
            // For each completed primary production in each state, identify the
            // production rule's index and insert a reduction on each of the LHS's
            // follow set
            for (primary_productions_table.items, 0..) |row, state_num| {
                for (row) |instance| {
                    if (instance.readCursor() != null) continue;

                    const rule_idx = grammar.getRuleIdx(instance.production).?;
                    for (follow_set[instance.production.lhs.variable], 0..) |isFollow, t_idx| {
                        if (!isFollow) continue;

                        action_table.items[state_num][t_idx] = try switch (action_table.items[state_num][t_idx]) {
                            .invalid => Action{ .reduce = rule_idx },
                            .state => TableGeneratorError.shiftReduceError,
                            .reduce => TableGeneratorError.reduceReduceError,
                            .accept => TableGeneratorError.reduceAcceptError,
                        };
                    }

                    // TODO: hardcoded 0 kinda yucky
                    if (instance.production.lhs.variable == grammar.getStartVariableIdx()) {
                        switch (action_table.items[state_num][grammar.getEndTerminalIdx()]) {
                            .invalid => return TableGeneratorError.acceptError,
                            .state => return TableGeneratorError.shiftAcceptError,
                            .reduce => |reduction_rule_idx| {
                                if (reduction_rule_idx == grammar.getStartRuleIdx()) {
                                    action_table.items[state_num][grammar.getEndTerminalIdx()] = Action.accept;
                                } else {
                                    return TableGeneratorError.acceptError;
                                }
                            },
                            .accept => {}
                            
                        }
                    }
                }
            }
            return .{try goto_table.toOwnedSlice(), try action_table.toOwnedSlice()};
        }
    };
} 

test "ParseTable.expandProductions-grammar1_0" {
    const V = TestVariable.fromString;
    const G = Grammar(TestVariable, TestToken);
    comptime var grammar = G.initFromTuples(
        .{
            .{V("S"), .{V("wff")}},
            .{V("wff"), .{TestToken.Proposition}},
            .{V("wff"), .{TestToken.Not,    V("wff")}},
            .{V("wff"), .{TestToken.LParen, V("wff"), TestToken.And,    V("wff"), TestToken.RParen}},
            .{V("wff"), .{TestToken.LParen, V("wff"), TestToken.Or,     V("wff"), TestToken.RParen}},
            .{V("wff"), .{TestToken.LParen, V("wff"), TestToken.Cond,   V("wff"), TestToken.RParen}},
            .{V("wff"), .{TestToken.LParen, V("wff"), TestToken.Bicond, V("wff"), TestToken.RParen}},
        },
        V("S"),
        TestToken.End
    );
    defer grammar.deinit();

    const P = ParseTable(TestVariable, TestToken);

    const start_productions = [_]ProductionInstance{ProductionInstance.fromProduction(grammar.rules[0])};
    const variable_branches, const terminal_branches = try P.expandProductions(std.testing.allocator, grammar, &start_productions);
    defer {
        for (variable_branches) |variable_list| {
            variable_list.deinit();
        }
        for (terminal_branches) |terminal_list| {
            terminal_list.deinit();
        }
        std.testing.allocator.free(variable_branches);
        std.testing.allocator.free(terminal_branches);
    }

    debug.print("\nVARIABLES:\n", .{});
    for (variable_branches, 0..) |variable_prods, i| {
        debug.print("{d}\n", .{i});
        for (variable_prods.items) |p| {
            try grammar.printDebugProductionInstance(p);
        }
    }
    debug.print("\nTERMINALS:\n", .{});
    for (terminal_branches, 0..) |terminal_prods, i| {
        debug.print("{d}\n", .{i});
        for (terminal_prods.items) |p| {
            try grammar.printDebugProductionInstance(p);
        }
    }
}

test "create_parse_table-grammar1_0" {
    const V = TestVariable.fromString;
    const G = Grammar(TestVariable, TestToken);
    comptime var grammar = G.initFromTuples(
        .{
            .{V("S"), .{V("wff")}},
            .{V("wff"), .{TestToken.Proposition}},
            .{V("wff"), .{TestToken.Not,    V("wff")}},
            .{V("wff"), .{TestToken.LParen, V("wff"), TestToken.And,    V("wff"), TestToken.RParen}},
            .{V("wff"), .{TestToken.LParen, V("wff"), TestToken.Or,     V("wff"), TestToken.RParen}},
            .{V("wff"), .{TestToken.LParen, V("wff"), TestToken.Cond,   V("wff"), TestToken.RParen}},
            .{V("wff"), .{TestToken.LParen, V("wff"), TestToken.Bicond, V("wff"), TestToken.RParen}},
        },
        V("S"),
        TestToken.End
    );
    defer grammar.deinit();

    const P = ParseTable(TestVariable, TestToken);

    const table = try P.init(std.testing.allocator, grammar);
    defer table.deinit();
    
    table.printDebugTable();
}