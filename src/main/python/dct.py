import argparse
import datetime
import pathlib
import regex

date_pattern = r'''(?:
    (?P<month>\d+)/(?P<day>\d+)/(?P<year>\d+)
    |
    (?P<day>\d+)-?(?P<month>\w{3})-?(?P<year>\d+)
    |
    (?P<month>\w{3})-(?P<day>\d+)-(?P<year>\d+)
    |
    (?P<month>\w+)\s+(?P<day>\d+),\s+(?P<year>\d+)
)'''
patterns = [regex.compile(p, flags=regex.VERBOSE | regex.DOTALL) for p in [
    fr'^\s*\[meta\s*rev_date="{date_pattern}',
    fr'Electronically\s*Signed:\s*{date_pattern}',
    fr'Revision\s*History\s*{date_pattern}',
    fr'{date_pattern}\s*\d{2}:\d{2}\s*Interpreted\s*by',
    fr'Discharge\s*information\s*provided\s*{date_pattern}',
    fr'Information\s*provided\s*{date_pattern}',
    fr'^\s*{date_pattern}',
    fr'Allergies\s*above\s*current\s*as\s*of\s*\w+,\s*{date_pattern}'
]]


def find_latest_date_and_evidence(text: str) -> (datetime.date, list[str]):
    dates = []
    evidence = []
    for pattern in patterns:
        for match in pattern.finditer(text):
            evidence.append(match.group())
            day = int(match.group("day"))
            month = match.group("month")
            if month.isdigit():
                month = int(month)
            elif len(month) == 3:
                month = datetime.datetime.strptime(month, "%b").month
            else:
                month = datetime.datetime.strptime(month, "%B").month
            year = int(match.group("year"))
            dates.append(datetime.date(year, month, day))
    dates.sort()
    return dates[-1] if dates else None, evidence


def write_dct_files(input_dir: pathlib.Path, output_dir: pathlib.Path):
    output_dir.mkdir(parents=True, exist_ok=True)
    for path in input_dir.glob("**/*"):
        if path.is_file() and not path.match(".*"):
            date, evidence = find_latest_date_and_evidence(path.read_text())
            if date is None:
                prompt = f'Please read {path} and manually enter DCT:'
                date = datetime.date.fromisoformat(input(prompt))
            output_dir.joinpath(f"{path.name}.dct").write_text(date.isoformat())


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("input_dir", type=pathlib.Path)
    parser.add_argument("output_dir", type=pathlib.Path)
    args = parser.parse_args()
    write_dct_files(**vars(args))
