import anafora
import argparse
import os
from collections import Counter


def main(input_dir, exclude, verbose):
    duplicate_types = Counter()
    parent_types = Counter()
    paths = anafora.walk(input_dir, xml_name_regex=r'TimeNorm\.gold\.completed')
    for sub_dir, text_file_name, xml_file_names in paths:
        for xml_file_name in xml_file_names:
            input_path = os.path.join(input_dir, sub_dir, xml_file_name)
            data = anafora.AnaforaData.from_file(input_path)

            # find entities that share the same type and span
            counts = Counter()
            for entity in data.annotations:
                if entity.type not in exclude:
                    counts[entity.spans, entity.type] += 1
            duplicates = {key for key, count in counts.items() if count > 1}

            # which types are most often duplicated
            if duplicates:
                if verbose:
                    print(f"{xml_file_name}")
                for entity_spans, entity_type in sorted(duplicates):
                    if verbose:
                        print(f"  {entity_spans} {entity_type}")
                    duplicate_types[entity_type] += 1

            # which types most often have duplicated entities as arguments
            for entity in data.annotations:
                for _, value in entity.properties.items():
                    if isinstance(value, anafora.AnaforaAnnotation) and \
                            (value.spans, value.type) in duplicates:
                        if verbose:
                            print(f"  parent: {entity.spans} {entity.type} of "
                                  f"{value.spans} {value.type}")
                        parent_types[entity.type] += 1
                        break

    print(f'duplicate types: {duplicate_types}')
    print(f'parent types:    {parent_types}')


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("input_dir")
    parser.add_argument("--exclude", nargs="+")
    parser.add_argument("--verbose", action='store_true')
    args = parser.parse_args()

    main(**vars(args))
