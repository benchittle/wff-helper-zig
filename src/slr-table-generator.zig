const std = @import("std");
const debug = std.debug;

const stdout = std.io.getStdOut().writer();

const lex = @import("wff-lexing.zig");

pub const TableGeneratorError = error{
    shiftShiftError,
};

const Variable = struct {
    const Self = @This();

    id: usize,

    fn eql(self: Self, other: Self) bool {
        return self.id == other.id;
    }
};

const Terminal = struct {
    const Self = @This();

    id: usize,

    fn eql(self: Self, other: Self) bool {
        return self.id == other.id;
    }
};

const Symbol = union(enum) {
    const Self = @This();

    variable: Variable,
    terminal: Terminal,

    fn eql(self: Self, other: Self) bool {
        return switch(self) {
            .variable => |v1| switch(other) {
                .variable => |v2| v1.eql(v2),
                .terminal => |_| false,
            },
            .terminal => |t1| switch(other) {
                .variable => |_| false,
                .terminal => |t2| t1.eql(t2),
            }
        };
    }

    fn getId(self: Self) usize {
        return switch(self) {
            .variable => |v| v.id,
            .terminal => |t| t.id,
        };
    }
};

const Production = struct {
    // TODO: Both of these helper structs should probably have dynamically 
    // allocated strings
    const Self = @This();
    
    lhs: Variable,
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

// NOTE: variables ids MUST START AT 0 and MUST BE SMALLER THAN ALL TERMINAL IDS
const Grammar = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    rules: []const Production,
    variables: []const usize,
    terminals: []const usize,

    firsts: [][]const bool,

    fn getOffsetTerminalId(self: Self, id: usize) usize {
        return id - self.variables.len;
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
            seen[v] = true;

            var stack = std.ArrayList(usize).init(self.allocator);
            defer stack.deinit();
            try stack.append(v);

            while (stack.popOrNull()) |top| {
                for (self.rules) |rule| {
                    // const first_rule_symbol = rule.rhs[0].getId();
                    switch(rule.rhs[0]) {
                        .terminal => |t| {
                            if (rule.lhs.id == top) {
                                firsts[v][self.getOffsetTerminalId(t.id)] = true;
                            }
                        },
                        .variable => |v2| {
                            if (rule.lhs.id == top and !seen[v2.id]) {
                                if (v2.id < v) { // entry already populated
                                    for (firsts[v2.id], 0..) |isInFirstList, i| {
                                        if (isInFirstList) {
                                            firsts[v][i] = true;
                                        }
                                    }
                                } else {
                                    try stack.append(v2.id);
                                }
                                seen[v2.id] = true;
                            }
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

    fn idIsVariable(self: Self, id: usize) bool {
        for (self.variables) |v| {
            if (v == id) return true;
        }
        return false;
    }

    fn idIsTerminal(self: Self, id: usize) bool {
        return !self.idIsVariable(id);
    }

    pub fn printProductionInstance(_: Self, prod: ProductionInstance) !void {
        try stdout.print("{d}", .{prod.production.lhs.id});
        try stdout.print(" ->", .{});
        for (prod.production.rhs[0..prod.cursor]) |sym| {
            switch(sym) {
                .terminal => |t| try stdout.print(" {d}", .{t.id}),
                .variable => |v| try stdout.print(" {d}", .{v.id}),
            }
        }
        try stdout.print(" *", .{});
        for (prod.production.rhs[prod.cursor..]) |sym| {
            switch(sym) {
                .terminal => |t| try stdout.print(" {d}", .{t.id}),
                .variable => |v| try stdout.print(" {d}", .{v.id}),
            }
        }
        try stdout.print("\n", .{});
    }
};

test "firsts" {
    // R1: (0) -> (1)
    // R2: (0) -> 4
    // R3: (0) -> (3)
    // R4: (1) -> (2)
    // R6: (2) -> (1)
    // R7: (2) -> 5
    // R8: (3) -> 6
    
    // R5: (2) -> (0)
    var r1 = Production{
        .lhs = Variable{.id = 0},
        .rhs = &[_]Symbol{Symbol{.variable = Variable{.id = 1}}, 
        }
    };

    var r2 = Production{
        .lhs = Variable{.id = 0},
        .rhs = &[_]Symbol{Symbol{.terminal = Terminal{.id = 4}}, 
        }
    };

    var r3 = Production{
        .lhs = Variable{.id = 0},
        .rhs = &[_]Symbol{Symbol{.variable = Variable{.id = 3}}, 
        }
    };

    var r4 = Production{
        .lhs = Variable{.id = 1},
        .rhs = &[_]Symbol{Symbol{.variable = Variable{.id = 2}}, 
        }
    };

    // var r5 = Production{
    //     .lhs = Variable{.id = 2},
    //     .rhs = &[_]Symbol{Symbol{.variable = Variable{.id = 0}}, 
    //     }
    // };

    var r6 = Production{
        .lhs = Variable{.id = 2},
        .rhs = &[_]Symbol{Symbol{.variable = Variable{.id = 1}}, 
        }
    };

    var r7 = Production{
        .lhs = Variable{.id = 2},
        .rhs = &[_]Symbol{Symbol{.terminal = Terminal{.id = 5}}, 
        }
    };

    var r8 = Production{
        .lhs = Variable{.id = 3},
        .rhs = &[_]Symbol{Symbol{.terminal = Terminal{.id = 6}}, 
        }
    };

    var grammar = Grammar{
        .allocator = std.testing.allocator,
        .variables = &[_] usize{0, 1, 2, 3},
        .terminals = &[_] usize{4, 5, 6},
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
            switch (prod.readCursor() orelse continue) {
                .variable => |v| {
                    if (branches[v.id].items.len == 0) {
                        for (self.grammar.rules) |rule| {
                            if (v.eql(rule.lhs)) {
                                try stack.append(ProductionInstance.fromProduction(rule));
                            }
                        }
                    }
                    try branches[v.id].append(prod.copyAdvanceCursor());
                },
                .terminal => |t| try branches[t.id].append(prod.copyAdvanceCursor()), 
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
        .lhs = Variable{.id = 0},
        .rhs = &[_]Symbol{Symbol{.terminal = Terminal{.id = 1}}}
    };

    var r2 = Production{
        .lhs = Variable{.id = 0},
        .rhs = &[_]Symbol{
            Symbol{.terminal = Terminal{.id = 2}}, 
            Symbol{.variable = Variable{.id = 0}}
        }
    };

    var r3 = Production{
        .lhs = Variable{.id = 0},
        .rhs = &[_]Symbol{
            Symbol{.terminal = Terminal{.id = 3}}, 
            Symbol{.variable = Variable{.id = 0}},
            Symbol{.terminal = Terminal{.id = 5}}, 
            Symbol{.variable = Variable{.id = 0}},
            Symbol{.terminal = Terminal{.id = 4}}, 
        }
    };

    var r4 = Production{
        .lhs = Variable{.id = 0},
        .rhs = &[_]Symbol{
            Symbol{.terminal = Terminal{.id = 3}}, 
            Symbol{.variable = Variable{.id = 0}},
            Symbol{.terminal = Terminal{.id = 6}}, 
            Symbol{.variable = Variable{.id = 0}},
            Symbol{.terminal = Terminal{.id = 4}}, 
        }
    };

    var r5 = Production{
        .lhs = Variable{.id = 0},
        .rhs = &[_]Symbol{
            Symbol{.terminal = Terminal{.id = 3}}, 
            Symbol{.variable = Variable{.id = 0}},
            Symbol{.terminal = Terminal{.id = 7}}, 
            Symbol{.variable = Variable{.id = 0}},
            Symbol{.terminal = Terminal{.id = 4}}, 
        }
    };

    var r6 = Production{
        .lhs = Variable{.id = 0},
        .rhs = &[_]Symbol{
            Symbol{.terminal = Terminal{.id = 3}}, 
            Symbol{.variable = Variable{.id = 0}},
            Symbol{.terminal = Terminal{.id = 8}}, 
            Symbol{.variable = Variable{.id = 0}},
            Symbol{.terminal = Terminal{.id = 4}}, 
        }
    };


    var r0 = Production{
        .lhs = Variable{.id = 9},
        .rhs = &[_] Symbol{Symbol{.variable = Variable{.id = 0}}}
    };

    var grammar = Grammar {
        .allocator = std.testing.allocator,
        .rules = &[_] Production{r1, r2, r3, r4, r5, r6},
        .variables = &[_] usize{0},
        .terminals = &[_] usize{1, 2, 3, 4, 5, 6, 7, 8},
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

