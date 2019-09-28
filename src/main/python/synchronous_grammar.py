import re


class SynchronousGrammar(object):

    def __init__(self, root_symbols, rules):
        self.root_symbols = root_symbols
        self.rules = rules
        self._rule_prefix_map = _PrefixMultiMap()
        self._number_ranges = []
        for rule in self.rules:
            self._rule_prefix_map.add(rule.sources, rule)
            for symbol in [rule.symbol] + rule.sources:
                match_number_ranges = re.match(r"^\[Int:(\d*)-(\d*)\]$", symbol)
                if match_number_ranges is not None:
                    begin = int(match_number_ranges.group(1))
                    end = int(match_number_ranges.group(2))
                    number_range = list(range(begin, end+1))
                    self._number_ranges.append(number_range)

    def source_symbols_for_number(self, number):
        symbols_with_ranges = []
        for number_range in self._number_ranges:
            if number in number_range:
                symbols_with_ranges.append("[Int:%d-%d]" % (number_range[0], number_range[-1]))
        symbols_with_ranges.append("[Int]")
        return symbols_with_ranges

    def source_seq_startswith(self, tokens):
        if not isinstance(tokens, list):
            tokens = [tokens]
        return self._rule_prefix_map.get_all_with_prefix(tokens)

    def source_symbols(self):
        source_symbol_list = []
        for rule in self.rules:
            source_symbol_list.extend(rule.sources)
        return source_symbol_list


class _PrefixMultiMap(object):

    def __init__(self):
        self.suffixes = {}
        self.values = []

    def add(self, keys, value):
        if not bool(keys):
            self.values.append(value)
        else:
            head = keys[0]
            if head not in self.suffixes:
                self.suffixes[head] = _PrefixMultiMap()
            self.suffixes[head].add(keys[1:], value)

    def _get_map(self, keys):
        if not bool(keys):
            return self
        else:
            head = keys[0]
            if head in self.suffixes:
                return self.suffixes[head]._get_map(keys[1:])
            else:
                return None

    def get(self, keys):
        prefix_multi_map = self._get_map(keys)
        if prefix_multi_map is None:
            return []
        else:
            return prefix_multi_map.values

    def get_all(self):
        all_values = []
        all_values.extend(self.values)
        for suffix_values in self.suffixes.values():
            all_values.extend(suffix_values.get_all())
        return all_values

    def get_all_with_prefix(self, keys):
        prefix_multi_map = self._get_map(keys)
        if prefix_multi_map is None:
            return []
        else:
            return prefix_multi_map.get_all()


class Rule(object):

    def __init__(self, symbol, sources, targets, non_terminal_alignment):
        self.symbol = symbol
        self.sources = sources
        self.targets = targets
        self.non_terminal_alignment = non_terminal_alignment
        self.basic_symbol = basic_symbol(self.symbol)
        self.is_nil = is_nil(self.symbol)


def is_terminal(token):
    return not re.search(r"^\[.*\]$", token)


def is_number(token):
    return re.search(r"^\d+$", token)


def basic_symbol(token):
    return re.sub(r":[^\]]*", "", token)


def is_nil(token):
    return basic_symbol(token) == "[Nil]"


def strip_label(label):
    return re.sub(r'\[([^\[\]]*),[^\[\]]*\]', r"[\1]", label)


def load_grammar(grammar_path):
    with open(grammar_path) as grammar_file:
        lines = grammar_file.read().splitlines()
        first_line = lines[0]
        assert first_line.startswith("ROOTS"), "First line must define root symbols, e.g. ROOTS [XXX] [YYY]"
        root_symbols = re.split(r"\s+", first_line)[1:]
        rules = []
        for line in lines[1:]:
            if not (line.startswith("//") or line == ""):
                symbol, sources_string, targets_string, score_string = re.split(r"\s*[|][|][|]\s*", line)
                sources = re.split("\\s+", sources_string)
                targets = re.split("\\s+", targets_string)
                source_non_terminals = list(filter(lambda t: not is_terminal(t), sources))
                target_non_terminals = list(filter(lambda t: not is_terminal(t), targets))
                alignment = {}
                for target_idx, token in enumerate(target_non_terminals):
                    if source_non_terminals.count(token) != 1:
                        raise ValueError("Expected exactly 1 non-terminal matching \"%s\" in %s" % (token, sources))
                    alignment[target_idx] = source_non_terminals.index(token)
                sources = list(map(strip_label, sources))
                targets = list(map(strip_label, targets))
                rule = Rule(symbol, sources, targets, alignment)
                rules.append(rule)
        return SynchronousGrammar(root_symbols, rules)
