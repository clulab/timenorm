import re
from collections import deque

import synchronous_grammar as sg


class SynchronousParser(object):

    def __init__(self, grammar):
        self.grammar = grammar

    def parse_chart(self, source_tokens):
        num_tokens = len(source_tokens)
        chart = []
        for size in range(num_tokens + 1):
            chart_row = []
            for start in range(num_tokens):
                if size == 0 or start + size > num_tokens:
                    chart_row.append(None)
                else:
                    chart_row.append(_ChartEntry())
            chart.append(chart_row)

        # special handling of [Number]: pass through tokens that are numbers
        for start in range(num_tokens):
            token = source_tokens[start]
            if sg.is_number(token):
                for symbol in self.grammar.source_symbols_for_number(int(token)):
                    rule = sg.Rule(symbol, [token], [token], {})
                    chart[1][start].completes.append(_Parse(rule, []))

        # fill rules that start with terminals
        for start in range(num_tokens):
            for rule in self.grammar.source_seq_startswith(source_tokens[start]):
                initial_terminals = []
                sources_iterator = iter(rule.sources)
                next_source = next(sources_iterator)
                while sg.is_terminal(next_source):
                    try:
                        initial_terminals.append(next_source)
                        next_source = next(sources_iterator)
                    except StopIteration:
                        break
                size = len(initial_terminals)
                if source_tokens[start: start + size] == initial_terminals:
                    entry = chart[size][start]
                    if len(rule.sources) == size:
                        entry.completes.append(_Parse(rule, []))
                    else:
                        entry.partials.append(_PartialParse(rule, size, []))

        # fill in the chart from the smallest sizes to the biggest size
        for size in range(1, num_tokens + 1):
            for start in range(0, num_tokens - size + 1):
                entry = chart[size][start]

                # look for ways to create entries of size `size` from the current partial parses
                for size_1 in range(1, size):
                    start_2 = start + size_1
                    size_2 = size - size_1
                    for partial in chart[size_1][start].partials:

                        # partials that can be advanced to `size` using terminals
                        new_source_seq_index = partial.source_seq_index + size_2
                        symbols = partial.rule.sources[partial.source_seq_index: new_source_seq_index]
                        tokens = source_tokens[start_2: start_2 + size_2]
                        if all(map(sg.is_terminal, symbols)) and symbols == tokens:
                            if len(partial.rule.sources) == new_source_seq_index:
                                entry.completes.append(_Parse(partial.rule, partial.non_terminal_rules))
                            else:
                                entry.partials.append(_PartialParse(partial.rule, new_source_seq_index, partial.non_terminal_rules))

                        # partials that can be advanced to `size` using completed non-terminals
                        for complete in chart[size_2][start_2].completes:
                            if partial.rule.sources[partial.source_seq_index] == complete.rule.symbol:
                                source_seq_index = partial.source_seq_index + 1
                                non_terminal_rules = partial.non_terminal_rules + [complete]
                                if len(partial.rule.sources) == source_seq_index:
                                    entry.completes.append(_Parse(partial.rule, non_terminal_rules))
                                else:
                                    entry.partials.append(_PartialParse(partial.rule, source_seq_index, non_terminal_rules))

                    # expand complete parses if there are Nil parses beside them
                    for complete_1 in chart[size_1][start].completes:
                        for complete_2 in chart[size_2][start_2].completes:
                            if not complete_1.rule.is_nil and complete_2.rule.is_nil:
                                entry.completes.append(complete_1)
                            elif complete_1.rule.is_nil and not complete_2.rule.is_nil:
                                entry.completes.append(complete_2)

                # create parses for rules that start with any of the currently complete parses
                # NOTE: we have to use a queue here because the loop itself may add more completed
                # rules that we then also need to process
                queue = deque(entry.completes)
                while bool(queue):
                    complete = queue.popleft()
                    for rule in self.grammar.source_seq_startswith(complete.rule.symbol):
                        if not bool(rule.sources[1:]):
                            complete_2 = _Parse(rule, [complete])
                            queue.append(complete_2)
                            entry.completes.append(complete_2)
                        else:
                            entry.partials.append(_PartialParse(rule, 1, [complete]))

        return chart

    def parse_all(self, source_tokens):
        if not bool(source_tokens):
            raise TypeError("Cannot parse empty token sequence")
        chart = self.parse_chart(source_tokens)
        completes = chart[len(source_tokens)][0].completes
        roots = list(filter(lambda c: c.rule.symbol in self.grammar.root_symbols, completes))
        trees = [root.to_target_tree() for root in roots]
        if not bool(trees):
            num_tokens = len(source_tokens)
            completes = []
            for size in range(1, num_tokens + 1):
                for start in range(0, num_tokens - size):
                    for complete in chart[size][start].completes:
                        complete = "{}({})".format(complete.rule.symbol, ",".join(source_tokens[start: start + size]))
                        completes.append(complete)
            message = "Could not parse {}. Partial parses:\n{}"
            raise ValueError(message.format(source_tokens, "\n".join(completes)))
        return trees


class _Parse(object):

    def __init__(self, rule, non_terminal_rules):
        self.rule = rule
        self.non_terminal_rules = non_terminal_rules

    def to_target_tree(self):
        non_terminal_index = -1
        children = []
        for token_idx, token in enumerate(self.rule.targets):
            if sg.is_terminal(token):
                children.append(Terminal(token))
            else:
                non_terminal_index += 1
                non_terminal_rules_index = self.rule.non_terminal_alignment[non_terminal_index]
                children.append(self.non_terminal_rules[non_terminal_rules_index].to_target_tree())
        subtrees = self._insert_subtrees_from_parentheses(children)
        return NonTerminal(self.rule, subtrees)

    def _insert_subtrees_from_parentheses(self, trees):
        if not bool(trees):
            return []
        else:
            next_tree, *trees = trees
            if next_tree == Terminal("("):
                tree = self._parse_subtree_following_open_parentheses(trees)
            else:
                tree = next_tree
            return [tree] + self._insert_subtrees_from_parentheses(trees)

    def _parse_subtree_following_open_parentheses(self, trees):
        next_tree, *trees = trees
        children = []
        get_next = True
        while get_next:
            if next_tree == Terminal(")"):
                get_next = False
            elif next_tree == Terminal("("):
                children.append(self._parse_subtree_following_open_parentheses(trees))
            else:
                children.append(next_tree)
        rule = sg.Rule("[" + next_tree.symbol + "]", [], [], {})
        return NonTerminal(rule, children)


class Tree(object):
    pass


class Terminal(Tree):
    def __init__(self, token):
        self.token = token

    def __eq__(self, other):
        if isinstance(other, Terminal) and self.token == other.token:
            return True
        else:
            return False


class NonTerminal(Tree):
    def __init__(self, rule, children):
        self.token = rule
        self.children = children


class _PartialParse(object):
    def __init__(self, rule, source_seq_index, non_terminal_rules):
        self.rule = rule
        self.source_seq_index = source_seq_index
        self.non_terminal_rules = non_terminal_rules


class _ChartEntry(object):
    def __init__(self):
        self.completes = []
        self.partials = []


def to_digits(tree):
    digits = []
    if isinstance(tree, Terminal):
        # return [int(tree.token)]
        # return [tree.token]
        return tree.token
    elif isinstance(tree, NonTerminal):
        for child in tree.children:
            # digits.extend(to_digits(child))
            digits.append(to_digits(child))
    return digits


# parser = SynchronousParser(sg.load_grammar("../resources/org/clulab/timenorm/en.numbers.grammar"))
parser = SynchronousParser(sg.load_grammar("/home/egoitz/Home/Code/scala/geonorm/src/main/resources/en.grammar"))
#result = parser.parse_all(["the", "first", "week", "of", "1976"])

import sys
instr = sys.argv[1]
#instr = "the eastern slopes of the SHP , from SHP to SHP"
result = parser.parse_all(instr.split(" "))
result = to_digits(result[0])
print(result)
# result = to_digits(result[0])
# result.reverse()
# sum = 0
# for magnitude, digit in enumerate(result):
#     sum += 10 ** magnitude * digit
# print(sum)
