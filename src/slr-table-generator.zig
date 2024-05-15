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

const SymbolID = u32;

const Symbol = union(enum) {
    const Self = @This();

    id: SymbolID,

    fn eql(self: Self, other: Self) bool {
        return self.id == other.id;
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
        return ProductionInstance{.production = self.production, .cursor = self.cursor + 1};
    }
};

// NOTE: variables ids MUST START AT 0 and MUST BE SMALLER THAN ALL TERMINAL IDS and MUST BE ORDERED
// TODO: get rid of variable list, just use count of variables and terminals, rest can be inferred
// NOTE: comparison of symbols is not always done using .eql in below functions, should try to make this consistent
// NOTE: FOLLOW and FIRST sets generation code is way too nested, should ideally 
//       be broken down into smaller functions 
fn Grammar(comptime T: type) type { 
    return struct {
        const Self = @This();
        const TokenMapping: type = std.meta.Tuple{&[_]type {[]u8, T}};

        //allocator: std.mem.Allocator,
        rules: []const Production,
        variables: [][]const u8,
        terminals: []T,

        fn initFromRuleStrings(
            comptime rule_strings: [][]u8, 
            comptime variable_names: [][]u8, 
            comptime start_variable: []u8,
            comptime terminal_token_map: []TokenMapping,
        ) Self {
            comptime {
                var variables: [][]u8 = {};
                // Parse rule strings into integer ID's
                
            }
        }

        fn getOffsetTerminalId(self: Self, terminal: Symbol) usize {
            return terminal.id - self.variables;
        }

        // NOTE: Assumes $ is last terminal symbol ID and S' is the first symbol ID 
        //       (0). FOLLOW(S') will be initialized to {$}, which will then be
        //       propogated as needed
        fn getFollowSet(self: Self) ![][]const bool {
            var first_set = try self.getFirstSet();
            defer {
                for (first_set) |row| self.allocator.free(row);
                self.allocator.free(first_set);
            }

            var follow_set = try self.allocator.alloc([]bool, self.variables);
            errdefer self.allocator.free(follow_set);
            var num_follow_allocated: usize = 0;
            for (0..follow_set.len) |i| {
                follow_set[i] = try self.allocator.alloc(bool, self.terminals);
                for (0..follow_set[i].len) |j| {
                    follow_set[i][j] = false;
                } 
                num_follow_allocated += 1;
            }
            // Initialize FOLLOW(S') to include END symbol.
            follow_set[0][self.terminals - 1] = true;
            errdefer for (follow_set[0..num_follow_allocated]) |list| {
                self.allocator.free(list);
            };

            for (0..self.variables) |v| {
                var seen = try self.allocator.alloc(bool, self.variables);
                defer self.allocator.free(seen);
                for (0..seen.len) |i| {
                    seen[i] = false;
                }
                seen[v] = true; // redundant?

                var stack = std.ArrayList(SymbolID).init(self.allocator);
                defer stack.deinit();
                try stack.append(@intCast(v));

                while (stack.popOrNull()) |top| {
                    for (self.rules) |rule| {
                        for (rule.rhs, 0..) |symbol, symbol_index| {
                            if (symbol.id != top) {
                                continue;
                            }
                            if (symbol_index + 1 == rule.rhs.len) {
                                if (!seen[rule.lhs.id]) { // optimize: if already caluclated, dont redo
                                    if (rule.lhs.id < top) {
                                        for (follow_set[rule.lhs.id], 0..) |isFollow, offset_follow_symbol_id| {
                                            if (isFollow) {
                                                follow_set[v][offset_follow_symbol_id] = true;
                                            }
                                        }
                                    } else {
                                        try stack.append(rule.lhs.id);
                                    }
                                    seen[rule.lhs.id] = true;
                                }
                            } else if (self.symbolIsTerminal(rule.rhs[symbol_index + 1])) {
                                follow_set[v][self.getOffsetTerminalId(rule.rhs[symbol_index + 1])] = true;
                            } else {
                                for (first_set[symbol.id], 0..) |isFirst, offset_first_symbol_id| {
                                    if (isFirst) {
                                        follow_set[v][offset_first_symbol_id] = true;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            return follow_set;
        }

        // NOTE: $ (END character) can never be first
        fn getFirstSet(self: Self) ![][]bool {
            var firsts = try self.allocator.alloc([]bool, self.variables);
            errdefer self.allocator.free(firsts);
            var num_firsts_allocated: usize = 0;
            for (0..firsts.len) |i| {
                firsts[i] = try self.allocator.alloc(bool, self.terminals);
                for (0..firsts[i].len) |j| {
                    firsts[i][j] = false; // Set default state to false, including for $
                } 
                num_firsts_allocated += 1;
            }
            errdefer for (firsts[0..num_firsts_allocated]) |list| {
                self.allocator.free(list);
            };
            
            // Iterate over all variable ID's
            for (0..self.variables) |v| {
                var seen = try self.allocator.alloc(bool, self.variables);
                defer self.allocator.free(seen);
                for (0..seen.len) |i| {
                    seen[i] = false;
                }
                seen[v] = true;

                var stack = std.ArrayList(SymbolID).init(self.allocator);
                defer stack.deinit();
                try stack.append(@intCast(v));

                while (stack.popOrNull()) |top| {
                    for (self.rules) |rule| {
                        const first_rule_symbol = rule.rhs[0];
                        if (rule.lhs.id == top) {
                            if (self.symbolIsTerminal(first_rule_symbol)) {
                                firsts[v][self.getOffsetTerminalId(first_rule_symbol)] = true;
                            } else if (!seen[first_rule_symbol.id]) {
                                if (first_rule_symbol.id < v) { // entry already populated
                                    for (firsts[first_rule_symbol.id], 0..) |isInFirstList, i| {
                                        if (isInFirstList) {
                                            firsts[v][i] = true;
                                        }
                                    }
                                } else {
                                    try stack.append(first_rule_symbol.id);
                                }
                                seen[first_rule_symbol.id] = true;
                            }
                        }
                    }
                }
            }

            return firsts;
        }

        fn getSymbolCount(self: Self) usize {
            return self.variables + self.terminals;
        }

        fn getRuleId(self: Self, rule: Production) ?usize {
            for (self.rules, 0..) |known_rule, i| {
                if (known_rule.eql(rule)) {
                    return i;
                }
            }
            return null;
        }

        fn symbolIsVariable(self: Self, sym: Symbol) bool {
            return sym.id < self.variables;
        }

        fn symbolIsTerminal(self: Self, sym: Symbol) bool {
            return  self.variables <= sym.id and sym.id < self.getSymbolCount();
        }

        pub fn printProductionInstance(_: Self, prod: ProductionInstance) !void {
            try stdout.print("{d}", .{prod.production.lhs.id});
            try stdout.print(" ->", .{});
            for (prod.production.rhs[0..prod.cursor]) |sym| {
                try stdout.print(" {d}", .{sym.id});
            }
            try stdout.print(" *", .{});
            for (prod.production.rhs[prod.cursor..]) |sym| {
                try stdout.print(" {d}", .{sym.id});
            }
            try stdout.print("\n", .{});
        }
    };
}


test "firsts_and_follows_simple" {
    // R0: (0) -> (1) (augment)
    // R1: (1) -> (2)
    // R2: (1) -> 5
    // R3: (1) -> (4)
    // R4: (2) -> (3)
    // R6: (3) -> (2)
    // R7: (3) -> 6
    // R8: (4) -> 7
    
    // R5: (3) -> (1)
    // $ = 8
    var r0 = Production{
        .lhs = Symbol{.id = 0},
        .rhs = &[_]Symbol{Symbol{.id = 1}}
    };

    var r1 = Production{
        .lhs = Symbol{.id = 1},
        .rhs = &[_]Symbol{Symbol{.id = 2}}, 
    };

    var r2 = Production{
        .lhs = Symbol{.id = 1},
        .rhs = &[_]Symbol{Symbol{.id = 5}}, 
    };

    var r3 = Production{
        .lhs = Symbol{.id = 1},
        .rhs = &[_]Symbol{Symbol{.id = 4}}, 
    };

    var r4 = Production{
        .lhs = Symbol{.id = 2},
        .rhs = &[_]Symbol{Symbol{.id = 3}}, 
    };

    // var r5 = Production{
    //     .lhs = Symbol{.id = 2},
    //     .rhs = &[_]Symbol{Symbol{.id = 0}}, 
    //     
    // };

    var r6 = Production{
        .lhs = Symbol{.id = 3},
        .rhs = &[_]Symbol{Symbol{.id = 2}}, 
    };

    var r7 = Production{
        .lhs = Symbol{.id = 3},
        .rhs = &[_]Symbol{Symbol{.id = 6}}, 
    };

    var r8 = Production{
        .lhs = Symbol{.id = 4},
        .rhs = &[_]Symbol{Symbol{.id = 7}}, 
    };

    var grammar = Grammar{
        .allocator = std.testing.allocator,
        .variables = 5,
        .terminals = 4,
        .rules = &[_] Production{r0, r1, r2, r3, r4, r6, r7, r8},
    };

    var first_set = try grammar.getFirstSet();
    defer {
        for (0..first_set.len) |i| {
            grammar.allocator.free(first_set[i]);
        }
        grammar.allocator.free(first_set);
    }

    var expected_firsts = [_][4]bool {
        .{true, true, true, false},
        .{true, true, true, false},
        .{false, true, false, false},
        .{false, true, false, false},
        .{false, false, true, false}
    };
    for (first_set, expected_firsts) |actual, expected| {
        try std.testing.expectEqualSlices(bool, &expected, actual);
    }

    var follow = try grammar.getFollowSet();
    defer {
        for (0..follow.len) |i| {
            grammar.allocator.free(follow[i]);
        }
        grammar.allocator.free(follow);
    }

    var expected_follow = [_][4]bool {
        .{false, false, false, true},
        .{false, false, false, true},
        .{false, false, false, true},
        .{false, false, false, true},
        .{false, false, false, true}
    };
    for (follow, expected_follow) |actual, expected| {
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

test "firsts_and_follows_grammar1_0" {
    // S' = 0
    // wff = 1
    // Proposition = 2
    // Not = 3
    // LParen = 4
    // RParen = 5
    // And = 6
    // Or = 7
    // Cond = 8
    // Bicond = 9
    // $ = 10
    var r1 = Production{
        .lhs = Symbol{.id = 1},
        .rhs = &[_]Symbol{Symbol{.id = 2}}
    };

    var r2 = Production{
        .lhs = Symbol{.id = 1},
        .rhs = &[_]Symbol{
            Symbol{.id = 3}, 
            Symbol{.id = 1}
        }
    };

    var r3 = Production{
        .lhs = Symbol{.id = 1},
        .rhs = &[_]Symbol{
            Symbol{.id = 4}, 
            Symbol{.id = 1},
            Symbol{.id = 6}, 
            Symbol{.id = 1},
            Symbol{.id = 5}, 
        }
    };

    var r4 = Production{
        .lhs = Symbol{.id = 1},
        .rhs = &[_]Symbol{
            Symbol{.id = 4}, 
            Symbol{.id = 1},
            Symbol{.id = 7}, 
            Symbol{.id = 1},
            Symbol{.id = 5}, 
        }
    };

    var r5 = Production{
        .lhs = Symbol{.id = 1},
        .rhs = &[_]Symbol{
            Symbol{.id = 4}, 
            Symbol{.id = 1},
            Symbol{.id = 8}, 
            Symbol{.id = 1},
            Symbol{.id = 5}, 
        }
    };

    var r6 = Production{
        .lhs = Symbol{.id = 0},
        .rhs = &[_]Symbol{
            Symbol{.id = 4}, 
            Symbol{.id = 1},
            Symbol{.id = 9}, 
            Symbol{.id = 1},
            Symbol{.id = 5}, 
        }
    };


    var r0 = Production{
        .lhs = Symbol{.id = 0},
        .rhs = &[_] Symbol{Symbol{.id = 1}}
    };

    var grammar = Grammar {
        .allocator = std.testing.allocator,
        .rules = &[_] Production{r0, r1, r2, r3, r4, r5, r6},
        .variables = 2,
        .terminals = 9,
    };

    var first_set = try grammar.getFirstSet();
    defer {
        for (0..first_set.len) |i| {
            grammar.allocator.free(first_set[i]);
        }
        grammar.allocator.free(first_set);
    }

    var expected_firsts = [_][9]bool {
        .{true, true, true, false, false, false, false, false, false},
        .{true, true, true, false, false, false, false, false, false},
    };
    for (first_set, expected_firsts) |actual, expected| {
        try std.testing.expectEqualSlices(bool, &expected, actual);
    }
    
    var follow = try grammar.getFollowSet();
    defer {
        for (0..follow.len) |i| {
            grammar.allocator.free(follow[i]);
        }
        grammar.allocator.free(follow);
    }

    var expected_follow = [_][9]bool {
        .{false, false, false, false, false, false, false, false, true},
        .{false, false, false, true,  true, true, true, true, true},
    };
    for (follow, expected_follow) |actual, expected| {
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

test "follows_grammar2_2" {
    const V_S        = 0;
    const V_WFF1     = 1;
    const V_WFF2     = 2;
    const V_WFF3     = 3;
    const V_WFF4     = 4;
    const V_PROP     = 5;
    const T_BICOND   = 6;
    const T_COND     = 7;
    const T_OR       = 8;
    const T_AND      = 9;
    const T_NOT      = 10;
    const T_LPAREN   = 11;
    const T_RPAREN   = 12;
    const T_PROPTOK  = 13;
    // const T_DOLLAR   = 14;

    var r1 = Production{
        .lhs = Symbol{.id = V_S},
        .rhs = &[_]Symbol{Symbol{.id = V_WFF1}}
    };

    var r2 = Production{
        .lhs = Symbol{.id = V_WFF1},
        .rhs = &[_]Symbol{
            Symbol{.id = V_WFF1},
            Symbol{.id = T_BICOND}, 
            Symbol{.id = V_WFF2},
        }
    };

    var r3 = Production{
        .lhs = Symbol{.id = V_WFF2},
        .rhs = &[_]Symbol{Symbol{.id = V_WFF3}}
    };

    var r4 = Production{
        .lhs = Symbol{.id = V_WFF2},
        .rhs = &[_]Symbol{
            Symbol{.id = V_WFF2},
            Symbol{.id = T_COND}, 
            Symbol{.id = V_WFF3},
        }
    };

    var r5 = Production{
        .lhs = Symbol{.id = V_WFF3},
        .rhs = &[_]Symbol{Symbol{.id = V_WFF4}}
    };

    var r6 = Production{
        .lhs = Symbol{.id = V_WFF1},
        .rhs = &[_]Symbol{
            Symbol{.id = V_WFF3},
            Symbol{.id = T_OR}, 
            Symbol{.id = V_WFF4},
        }
    };

    var r7 = Production{
        .lhs = Symbol{.id = V_WFF3},
        .rhs = &[_]Symbol{
            Symbol{.id = V_WFF3},
            Symbol{.id = T_AND}, 
            Symbol{.id = V_WFF4},
        }
    };

    var r8 = Production{
        .lhs = Symbol{.id = V_WFF4},
        .rhs = &[_]Symbol{Symbol{.id = V_PROP}}
    };

    var r9 = Production{
        .lhs = Symbol{.id = V_WFF4},
        .rhs = &[_]Symbol{
            Symbol{.id = T_NOT},
            Symbol{.id = V_WFF4}, 
        }
    };

    var r10 = Production{
        .lhs = Symbol{.id = V_PROP},
        .rhs = &[_]Symbol{
            Symbol{.id = T_LPAREN},
            Symbol{.id = V_WFF1}, 
            Symbol{.id = T_RPAREN},
        }
    };

    var r11 = Production{
        .lhs = Symbol{.id = V_PROP},
        .rhs = &[_]Symbol{
            Symbol{.id = T_PROPTOK},
        }
    };


    var r0 = Production{
        .lhs = Symbol{.id = V_S},
        .rhs = &[_] Symbol{Symbol{.id = V_WFF1}}
    };

    var grammar = Grammar {
        .allocator = std.testing.allocator,
        .rules = &[_] Production{r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11},
        .variables = 6,
        .terminals = 9,
    };

    var first_set = try grammar.getFirstSet();
    defer {
        for (0..first_set.len) |i| {
            grammar.allocator.free(first_set[i]);
        }
        grammar.allocator.free(first_set);
    }

    var expected_firsts = [_][9]bool {
        .{false, false, false, false, true, true, false, true, false},
        .{false, false, false, false, true, true, false, true, false},
        .{false, false, false, false, true, true, false, true, false},
        .{false, false, false, false, true, true, false, true, false},
        .{false, false, false, false, true, true, false, true, false},
        .{false, false, false, false, false, true, false, true, false},
    };
    for (first_set, expected_firsts) |actual, expected| {
        try std.testing.expectEqualSlices(bool, &expected, actual);
    }

    var follow = try grammar.getFollowSet();
    defer {
        for (0..follow.len) |i| {
            grammar.allocator.free(follow[i]);
        }
        grammar.allocator.free(follow);
    }

    var expected_follow = [_][9]bool {
        .{false, false, false, false, false, false, false, false, true},
        .{true, false, false, false, false, false, true, false, true},
        .{true, true, false, false, false, false, true, false, true},
        .{true, true, true, true, false, false, true, false, true},
        .{true, true, true, true, false, false, true, false, true},
        .{true, true, true, true, false, false, true, false, true},
    };
    for (follow, expected_follow) |actual, expected| {
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


const ParseTable = struct {
    const Self = @This();
    const Action = union(enum) {
        state: usize,  
        reduce: usize, // index of grammar.rules
        invalid,
        accept,
    };

    grammar: Grammar,
    action_goto_table: [][]Action,

    fn expandProductions(self: Self, allocator: std.mem.Allocator, productions: []const ProductionInstance) ![]std.ArrayList(ProductionInstance) {
        // const? 
        var branches = try allocator.alloc(std.ArrayList(ProductionInstance), self.grammar.getSymbolCount());
        errdefer allocator.free(branches);
        
        for (branches, 0..) |_, i| {
            branches[i] = std.ArrayList(ProductionInstance).init(allocator);
        }
        errdefer {
            for (branches) |list| {
                list.deinit();
            }
            allocator.free(branches);
        }

        var stack = std.ArrayList(ProductionInstance).init(allocator);
        defer stack.deinit();

        try stack.appendSlice(productions);

        // Expand all given ProductionInstances and track all of the symbols
        // currently being read.
        while (stack.popOrNull()) |prod| {
            if (prod.readCursor()) |sym| {
                if (self.grammar.symbolIsVariable(sym)) {
                    if (branches[sym.id].items.len == 0) {
                        for (self.grammar.rules) |rule| {
                            if (sym.eql(rule.lhs)) {
                                try stack.append(ProductionInstance.fromProduction(rule));
                            }
                        }
                    }
                    try branches[sym.id].append(prod.copyAdvanceCursor());
                } else {
                    try branches[sym.id].append(prod.copyAdvanceCursor());
                }
            }
        }

        return branches;
    }

    fn checkProductionsAlreadyExpanded(starting_productions_table: std.ArrayList([]ProductionInstance), productions: std.ArrayList(ProductionInstance)) ?usize {
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

    fn populate(self: *Self, allocator: std.mem.Allocator, augmented_production: Production) !void {
        // TODO: defers
        var action_goto_table = std.ArrayList([]Action).init(allocator);
        defer action_goto_table.deinit();
        errdefer for (action_goto_table.items) |row| {
            allocator.free(row);
        };
        var primary_productions_table = std.ArrayList([]ProductionInstance).init(allocator);
        defer {
            for (primary_productions_table.items) |row| {
                allocator.free(row);
            }
            primary_productions_table.deinit();
        }

        try action_goto_table.append(try allocator.alloc(Action, self.grammar.getSymbolCount()));
        try primary_productions_table.append(try allocator.alloc(ProductionInstance, 1));
        primary_productions_table.items[0][0] = ProductionInstance.fromProduction(augmented_production);

        var state: usize = 0;
        while (state < action_goto_table.items.len) : (state += 1) {
            var branches = try self.expandProductions(allocator, primary_productions_table.items[state]);
            defer {
                for (branches) |prod_list| {
                    prod_list.deinit();
                }
                allocator.free(branches);
            }
            for (branches, 0..) |*prod_list, id| {
                if (prod_list.items.len == 0) {
                    action_goto_table.items[state][id] = Action.invalid;
                } else if (checkProductionsAlreadyExpanded(primary_productions_table, prod_list.*)) |existing_state| {
                    action_goto_table.items[state][id] = try switch (action_goto_table.items[state][id]) {
                        .invalid => Action{.state = existing_state},
                        .state => TableGeneratorError.shiftShiftError,
                        .reduce => TableGeneratorError.shiftReduceError,
                        .accept => TableGeneratorError.shiftAcceptError, 
                    };
                } else {
                    var new_prod_list = try allocator.alloc(Action, self.grammar.getSymbolCount());
                    try action_goto_table.append(new_prod_list);
                    try primary_productions_table.append(try prod_list.toOwnedSlice());
                    action_goto_table.items[state][id] = Action{.state = action_goto_table.items.len - 1};
                }               
            }
        }

        var follow_set = try self.grammar.getFollowSet();
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

                var rule_id = self.grammar.getRuleId(instance.production).?;
                for (follow_set[instance.production.lhs.id], self.grammar.variables..) |isFollow, terminal_id| {
                    if (!isFollow) continue;

                    action_goto_table.items[state_num][terminal_id] = try switch(action_goto_table.items[state_num][terminal_id]) {
                        .invalid => Action{.reduce = rule_id},
                        .state => TableGeneratorError.shiftReduceError,
                        .reduce => TableGeneratorError.reduceReduceError,
                        .accept => TableGeneratorError.reduceAcceptError,
                    };
                }

                // TODO: hardcoded 0 kinda yucky
                if (instance.production.lhs.id == 0) {
                    switch(action_goto_table.items[state_num][self.grammar.terminals - 1]) {
                        .invalid, .accept => action_goto_table.items[state_num][self.grammar.terminals - 1] = Action.accept,
                        .state => return TableGeneratorError.shiftAcceptError,
                        .reduce => return TableGeneratorError.reduceAcceptError,
                    }
                }
            }
        } 



        self.action_goto_table = try action_goto_table.toOwnedSlice();
    }
};

test "expandProductions-grammar1_0" {
    // S' = 0
    // wff = 1
    // Proposition = 2
    // Not = 3
    // LParen = 4
    // RParen = 5
    // And = 6
    // Or = 7
    // Cond = 8
    // Bicond = 9
    // $ = 10

    var r1 = Production{
        .lhs = Symbol{.id = 1},
        .rhs = &[_]Symbol{Symbol{.id = 2}}
    };

    var r2 = Production{
        .lhs = Symbol{.id = 1},
        .rhs = &[_]Symbol{
            Symbol{.id = 3}, 
            Symbol{.id = 1}
        }
    };

    var r3 = Production{
        .lhs = Symbol{.id = 1},
        .rhs = &[_]Symbol{
            Symbol{.id = 4}, 
            Symbol{.id = 1},
            Symbol{.id = 6}, 
            Symbol{.id = 1},
            Symbol{.id = 5}, 
        }
    };

    var r4 = Production{
        .lhs = Symbol{.id = 1},
        .rhs = &[_]Symbol{
            Symbol{.id = 4}, 
            Symbol{.id = 1},
            Symbol{.id = 7}, 
            Symbol{.id = 1},
            Symbol{.id = 5}, 
        }
    };

    var r5 = Production{
        .lhs = Symbol{.id = 1},
        .rhs = &[_]Symbol{
            Symbol{.id = 4}, 
            Symbol{.id = 1},
            Symbol{.id = 8}, 
            Symbol{.id = 1},
            Symbol{.id = 5}, 
        }
    };

    var r6 = Production{
        .lhs = Symbol{.id = 0},
        .rhs = &[_]Symbol{
            Symbol{.id = 4}, 
            Symbol{.id = 1},
            Symbol{.id = 9}, 
            Symbol{.id = 1},
            Symbol{.id = 5}, 
        }
    };


    var r0 = Production{
        .lhs = Symbol{.id = 0},
        .rhs = &[_] Symbol{Symbol{.id = 1}}
    };

    var grammar = Grammar {
        .allocator = std.testing.allocator,
        .rules = &[_] Production{r0, r1, r2, r3, r4, r5, r6},
        .variables = 2,
        .terminals = 9,
    };
    
    var table = ParseTable{
        .grammar = grammar,
        .action_goto_table = undefined,
    };

    const start_productions = [_]ProductionInstance {ProductionInstance.fromProduction(r0)};
    var branches = try table.expandProductions(std.testing.allocator, &start_productions);
    defer {
        for (branches) |list| {
            list.deinit();
        }
        std.testing.allocator.free(branches);
    }
    
    for (branches, 0..) |prods, i| {
        debug.print("\n{d}\n", .{i});
        for (prods.items) |p| {
            try grammar.printProductionInstance(p);
        }
    }   

    try table.populate(std.testing.allocator, r0);
    defer {
        for (table.action_goto_table) |row| {
            std.testing.allocator.free(row);
        }
        std.testing.allocator.free(table.action_goto_table);
    }

    for (table.action_goto_table, 0..) |row, s| {
        debug.print("{d: >3} ||", .{s});
        for (row) |entry| switch(entry) {
            .state => |state_num| debug.print(" {d: ^3} |", .{state_num}),
            .reduce => |rule_num| debug.print("R{d: ^3} |", .{rule_num}),
            .accept => debug.print(" ACC |", .{}),
            .invalid => debug.print("     |", .{}),
        };
        debug.print("\n", .{});
    }
}

