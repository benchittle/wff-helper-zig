const std = @import("std");
const debug = std.debug;

const stdout = std.io.getStdOut().writer();

const lex = @import("wff-lexing.zig");

pub const TableGeneratorError = error{
    shiftShiftError,
};

const Symbol = union(enum) {
    const Self = @This();

    id: usize,

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
const Grammar = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    rules: []const Production,
    variables: []const Symbol,
    terminals: []const Symbol,

    firsts: [][]const bool,

    fn getOffsetTerminalId(self: Self, terminal: Symbol) usize {
        return terminal.id - self.variables.len;
    }


    fn getFollowSet(self: Self) ![][]const bool {
        var follow = try self.allocator.alloc([]bool, self.variables.len);
        errdefer self.allocator.free(follow);
        var num_follow_allocated: usize = 0;
        for (0..follow.len) |i| {
            follow[i] = try self.allocator.alloc(bool, self.terminals.len);
            for (0..follow[i].len) |j| {
                follow[i][j] = false;
            } 
            num_follow_allocated += 1;
        }
        errdefer for (follow[0..num_follow_allocated]) |list| {
            self.allocator.free(list);
        };

        for (self.variables) |v| {
            var seen = try self.allocator.alloc(bool, self.variables.len);
            defer self.allocator.free(seen);
            for (0..seen.len) |i| {
                seen[i] = false;
            }
            seen[v.id] = true; // redundant?

            var stack = std.ArrayList(Symbol).init(self.allocator);
            defer stack.deinit();
            try stack.append(v);

            while (stack.popOrNull()) |top| {
                for (self.rules) |rule| {
                    for (rule.rhs, 0..) |symbol, symbol_index| {
                        if (!symbol.eql(top)) {
                            continue;
                        }
                        if (symbol_index + 1 == rule.rhs.len) {
                            if (!seen[rule.lhs.id]) { // optimize: if already caluclated, dont redo
                                try stack.append(rule.lhs);
                                seen[rule.lhs.id] = true;
                            }
                        } else if (self.symbolIsTerminal(rule.rhs[symbol_index + 1])) {
                            follow[v.id][self.getOffsetTerminalId(rule.rhs[symbol_index + 1])] = true;
                        } else {
                            for (self.firsts[symbol.id], 0..) |isFirst, first_symbol_id| {
                                if (isFirst) {
                                    follow[v.id][first_symbol_id] = true;
                                }
                            }
                        }
                    }
                }
            }
        }
        return follow;
    }


    fn populateFirstsTable(self: *Self) !void {
        var firsts = try self.allocator.alloc([]bool, self.variables.len);
        errdefer self.allocator.free(firsts);
        var num_firsts_allocated: usize = 0;
        for (0..firsts.len) |i| {
            firsts[i] = try self.allocator.alloc(bool, self.terminals.len);
            for (0..firsts[i].len) |j| {
                firsts[i][j] = false;
            } 
            num_firsts_allocated += 1;
        }
        errdefer for (firsts[0..num_firsts_allocated]) |list| {
            self.allocator.free(list);
        };

        for (self.variables) |v| {
            var seen = try self.allocator.alloc(bool, self.variables.len);
            defer self.allocator.free(seen);
            for (0..seen.len) |i| {
                seen[i] = false;
            }
            seen[v.id] = true;

            var stack = std.ArrayList(Symbol).init(self.allocator);
            defer stack.deinit();
            try stack.append(v);

            while (stack.popOrNull()) |top| {
                for (self.rules) |rule| {
                    const first_rule_symbol = rule.rhs[0];
                    if (rule.lhs.id == top.id) {
                        if (self.symbolIsTerminal(first_rule_symbol)) {
                            firsts[v.id][self.getOffsetTerminalId(first_rule_symbol)] = true;
                        } else if (!seen[first_rule_symbol.id]) {
                            if (first_rule_symbol.id < v.id) { // entry already populated
                                for (firsts[first_rule_symbol.id], 0..) |isInFirstList, i| {
                                    if (isInFirstList) {
                                        firsts[v.id][i] = true;
                                    }
                                }
                            } else {
                                try stack.append(first_rule_symbol);
                            }
                            seen[first_rule_symbol.id] = true;
                        }
                    }
                }
            }
        }

        self.firsts = firsts;
    }

    fn getSymbolCount(self: Self) usize {
        return self.variables.len + self.terminals.len;
    }

    fn symbolIsVariable(self: Self, sym: Symbol) bool {
        return sym.id < self.variables.len;
    }

    fn symbolIsTerminal(self: Self, sym: Symbol) bool {
        return  self.variables.len <= sym.id and sym.id < self.getSymbolCount();
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

// TODO: add equality tests rather than just printing

test "firsts_simple" {
    // R1: (0) -> (1)
    // R2: (0) -> 4
    // R3: (0) -> (3)
    // R4: (1) -> (2)
    // R6: (2) -> (1)
    // R7: (2) -> 5
    // R8: (3) -> 6
    
    // R5: (2) -> (0)
    var r1 = Production{
        .lhs = Symbol{.id = 0},
        .rhs = &[_]Symbol{Symbol{.id = 1}}, 
    };

    var r2 = Production{
        .lhs = Symbol{.id = 0},
        .rhs = &[_]Symbol{Symbol{.id = 4}}, 
    };

    var r3 = Production{
        .lhs = Symbol{.id = 0},
        .rhs = &[_]Symbol{Symbol{.id = 3}}, 
    };

    var r4 = Production{
        .lhs = Symbol{.id = 1},
        .rhs = &[_]Symbol{Symbol{.id = 2}}, 
    };

    // var r5 = Production{
    //     .lhs = Symbol{.id = 2},
    //     .rhs = &[_]Symbol{Symbol{.id = 0}}, 
    //     
    // };

    var r6 = Production{
        .lhs = Symbol{.id = 2},
        .rhs = &[_]Symbol{Symbol{.id = 1}}, 
    };

    var r7 = Production{
        .lhs = Symbol{.id = 2},
        .rhs = &[_]Symbol{Symbol{.id = 5}}, 
    };

    var r8 = Production{
        .lhs = Symbol{.id = 3},
        .rhs = &[_]Symbol{Symbol{.id = 6}}, 
    };

    var grammar = Grammar{
        .allocator = std.testing.allocator,
        .variables = &[_] Symbol{.{.id=0}, .{.id=1}, .{.id=2}, .{.id=3}},
        .terminals = &[_] Symbol{.{.id=4}, .{.id=5}, .{.id=6}},
        .rules = &[_] Production{r1, r2, r3, r4, r6, r7, r8},
        .firsts = undefined,
    };

    try grammar.populateFirstsTable();
    defer {
        for (0..grammar.firsts.len) |i| {
            grammar.allocator.free(grammar.firsts[i]);
        }
        grammar.allocator.free(grammar.firsts);
    }

    debug.print("\n", .{});
    for (grammar.firsts, 0..) |list, i| {
        debug.print("({d}):", .{i});
        for (list, grammar.variables.len..) |isFirst, j| {
            if (isFirst) {
                debug.print(" {d}", .{j});
            }
        }
        debug.print("\n", .{});
    }
}
test "follows_simple" {
    // R1: (0) -> (1)
    // R2: (0) -> 4
    // R3: (0) -> (3)
    // R4: (1) -> (2)
    // R6: (2) -> (1)
    // R7: (2) -> 5
    // R8: (3) -> 6
    
    // R5: (2) -> (0)
    var r1 = Production{
        .lhs = Symbol{.id = 0},
        .rhs = &[_]Symbol{Symbol{.id = 1}}, 
    };

    var r2 = Production{
        .lhs = Symbol{.id = 0},
        .rhs = &[_]Symbol{Symbol{.id = 4}}, 
    };

    var r3 = Production{
        .lhs = Symbol{.id = 0},
        .rhs = &[_]Symbol{Symbol{.id = 3}}, 
    };

    var r4 = Production{
        .lhs = Symbol{.id = 1},
        .rhs = &[_]Symbol{Symbol{.id = 2}}, 
    };

    // var r5 = Production{
    //     .lhs = Symbol{.id = 2},
    //     .rhs = &[_]Symbol{Symbol{.id = 0}}, 
    //     
    // };

    var r6 = Production{
        .lhs = Symbol{.id = 2},
        .rhs = &[_]Symbol{Symbol{.id = 1}}, 
    };

    var r7 = Production{
        .lhs = Symbol{.id = 2},
        .rhs = &[_]Symbol{Symbol{.id = 5}}, 
    };

    var r8 = Production{
        .lhs = Symbol{.id = 3},
        .rhs = &[_]Symbol{Symbol{.id = 6}}, 
    };

    var grammar = Grammar{
        .allocator = std.testing.allocator,
        .variables = &[_] Symbol{.{.id=0}, .{.id=1}, .{.id=2}, .{.id=3}},
        .terminals = &[_] Symbol{.{.id=4}, .{.id=5}, .{.id=6}},
        .rules = &[_] Production{r1, r2, r3, r4, r6, r7, r8},
        .firsts = undefined,
    };

    try grammar.populateFirstsTable();
    defer {
        for (0..grammar.firsts.len) |i| {
            grammar.allocator.free(grammar.firsts[i]);
        }
        grammar.allocator.free(grammar.firsts);
    }

    var follow = try grammar.getFollowSet();
    defer {
        for (0..follow.len) |i| {
            grammar.allocator.free(follow[i]);
        }
        grammar.allocator.free(follow);
    }

    debug.print("\n", .{});
    for (follow, 0..) |list, i| {
        debug.print("({d}):", .{i});
        for (list, grammar.variables.len..) |isFollow, j| {
            if (isFollow) {
                debug.print(" {d}", .{j});
            }
        }
        debug.print("\n", .{});
    }
}

test "firsts_grammar1_0" {
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
        .variables = &[_] Symbol{.{.id=0}, .{.id=1}},
        .terminals = &[_] Symbol{.{.id=2}, .{.id=3}, .{.id=4}, .{.id=5}, .{.id=6}, .{.id=7}, .{.id=8}, .{.id=9}, },
        .firsts = undefined,
    };

    try grammar.populateFirstsTable();
    defer {
        for (0..grammar.firsts.len) |i| {
            grammar.allocator.free(grammar.firsts[i]);
        }
        grammar.allocator.free(grammar.firsts);
    }

    debug.print("\n", .{});
    for (grammar.firsts, 0..) |list, i| {
        debug.print("({d}):", .{i});
        for (list, grammar.variables.len..) |isFirst, j| {
            if (isFirst) {
                debug.print(" {d}", .{j});
            }
        }
        debug.print("\n", .{});
    }
}
test "follows_grammar1_0" {
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
        .variables = &[_] Symbol{.{.id=0}, .{.id=1}},
        .terminals = &[_] Symbol{.{.id=2}, .{.id=3}, .{.id=4}, .{.id=5}, .{.id=6}, .{.id=7}, .{.id=8}, .{.id=9}, },
        .firsts = undefined,
    };

    try grammar.populateFirstsTable();
    defer {
        for (0..grammar.firsts.len) |i| {
            grammar.allocator.free(grammar.firsts[i]);
        }
        grammar.allocator.free(grammar.firsts);
    }
    
    var follow = try grammar.getFollowSet();
    defer {
        for (0..follow.len) |i| {
            grammar.allocator.free(follow[i]);
        }
        grammar.allocator.free(follow);
    }

    debug.print("\n", .{});
    for (follow, 0..) |list, i| {
        debug.print("({d}):", .{i});
        for (list, grammar.variables.len..) |isFollow, j| {
            if (isFollow) {
                debug.print(" {d}", .{j});
            }
        }
        debug.print("\n", .{});
    }
}

test "firsts_grammar2_2" {
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
        .variables = &[_] Symbol{.{.id=V_S}, .{.id=V_WFF1}, .{.id=V_WFF2}, .{.id=V_WFF3}, .{.id=V_WFF4}, .{.id=V_PROP}},
        .terminals = &[_] Symbol{.{.id=T_BICOND}, .{.id=T_COND}, .{.id=T_OR}, .{.id=T_AND}, .{.id=T_NOT}, .{.id=T_LPAREN}, .{.id=T_RPAREN}, .{.id=T_PROPTOK}},
        .firsts = undefined,
    };

    try grammar.populateFirstsTable();
    defer {
        for (0..grammar.firsts.len) |i| {
            grammar.allocator.free(grammar.firsts[i]);
        }
        grammar.allocator.free(grammar.firsts);
    }

    debug.print("\n", .{});
    for (grammar.firsts, 0..) |list, i| {
        debug.print("({d}):", .{i});
        for (list, grammar.variables.len..) |isFirst, j| {
            if (isFirst) {
                debug.print(" {d}", .{j});
            }
        }
        debug.print("\n", .{});
    }
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
        .variables = &[_] Symbol{.{.id=V_S}, .{.id=V_WFF1}, .{.id=V_WFF2}, .{.id=V_WFF3}, .{.id=V_WFF4}, .{.id=V_PROP}},
        .terminals = &[_] Symbol{.{.id=T_BICOND}, .{.id=T_COND}, .{.id=T_OR}, .{.id=T_AND}, .{.id=T_NOT}, .{.id=T_LPAREN}, .{.id=T_RPAREN}, .{.id=T_PROPTOK}},
        .firsts = undefined,
    };

    try grammar.populateFirstsTable();
    defer {
        for (0..grammar.firsts.len) |i| {
            grammar.allocator.free(grammar.firsts[i]);
        }
        grammar.allocator.free(grammar.firsts);
    }

    var follow = try grammar.getFollowSet();
    defer {
        for (0..follow.len) |i| {
            grammar.allocator.free(follow[i]);
        }
        grammar.allocator.free(follow);
    }

    debug.print("\n", .{});
    for (follow, 0..) |list, i| {
        debug.print("({d}):", .{i});
        for (list, grammar.variables.len..) |isFollow, j| {
            if (isFollow) {
                debug.print(" {d}", .{j});
            }
        }
        debug.print("\n", .{});
    }
}


const ParseTable = struct {
    const Self = @This();
    const Action = union(enum) {
        state: usize,  
        reduce: usize, // index of grammar.rules
        invalid,
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
                    switch (action_goto_table.items[state][id]) {
                        .state => |_| return TableGeneratorError.shiftShiftError,
                        else => action_goto_table.items[state][id] = Action{.state = existing_state},
                    }
                } else {
                    var new_prod_list = try allocator.alloc(Action, self.grammar.getSymbolCount());
                    try action_goto_table.append(new_prod_list);
                    try primary_productions_table.append(try prod_list.toOwnedSlice());
                    action_goto_table.items[state][id] = Action{.state = action_goto_table.items.len - 1};
                }               
            }
        }

        

        self.action_goto_table = try action_goto_table.toOwnedSlice();
    }
};

test "expandProductions-grammar1_0" {
    // wff = 0
    // Proposition = 1
    // Not = 2
    // LParen = 3
    // RParen = 4
    // And = 5
    // Or = 6
    // Cond = 7
    // Bicond = 8
    // S' = 9

    var r1 = Production{
        .lhs = Symbol{.id = 0},
        .rhs = &[_]Symbol{Symbol{.id = 1}}
    };

    var r2 = Production{
        .lhs = Symbol{.id = 0},
        .rhs = &[_]Symbol{
            Symbol{.id = 2}, 
            Symbol{.id = 0}
        }
    };

    var r3 = Production{
        .lhs = Symbol{.id = 0},
        .rhs = &[_]Symbol{
            Symbol{.id = 3}, 
            Symbol{.id = 0},
            Symbol{.id = 5}, 
            Symbol{.id = 0},
            Symbol{.id = 4}, 
        }
    };

    var r4 = Production{
        .lhs = Symbol{.id = 0},
        .rhs = &[_]Symbol{
            Symbol{.id = 3}, 
            Symbol{.id = 0},
            Symbol{.id = 6}, 
            Symbol{.id = 0},
            Symbol{.id = 4}, 
        }
    };

    var r5 = Production{
        .lhs = Symbol{.id = 0},
        .rhs = &[_]Symbol{
            Symbol{.id = 3}, 
            Symbol{.id = 0},
            Symbol{.id = 7}, 
            Symbol{.id = 0},
            Symbol{.id = 4}, 
        }
    };

    var r6 = Production{
        .lhs = Symbol{.id = 0},
        .rhs = &[_]Symbol{
            Symbol{.id = 3}, 
            Symbol{.id = 0},
            Symbol{.id = 8}, 
            Symbol{.id = 0},
            Symbol{.id = 4}, 
        }
    };


    var r0 = Production{
        .lhs = Symbol{.id = 9},
        .rhs = &[_] Symbol{Symbol{.id = 0}}
    };

    var grammar = Grammar {
        .allocator = std.testing.allocator,
        .rules = &[_] Production{r1, r2, r3, r4, r5, r6},
        .variables = &[_] Symbol{.{.id=0}},
        .terminals = &[_] Symbol{.{.id=1}, .{.id=2}, .{.id=3}, .{.id=4}, .{.id=5}, .{.id=6}, .{.id=7}, .{.id=8}},
        .firsts = undefined,
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
            .invalid => debug.print("     |", .{}),
        };
        debug.print("\n", .{});
    }
}

