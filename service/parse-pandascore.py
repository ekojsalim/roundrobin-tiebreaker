import typer
import json
from operator import itemgetter

app = typer.Typer()


def parse_opponent(opponent):
    if opponent is None:
        return None
    else:
        id, name, image_url = itemgetter("id", "name", "image_url")(opponent)
        return {"id": id, "name": name, "image_url": image_url}


@app.command()
def parse_json(input_file: str, output_file: str):
    """
    Parse a pandascore api response and write it to a json file.
    """

    with open(input_file, 'r') as f:
        data = json.load(f)

    parsed_data = []
    for match in data:
        id, name, status, begin_at, end_at, results = itemgetter(
            "id", "name", "status", "begin_at", "end_at", "results")(match)
        match_dict = {
            "id": id,
            "name": name,
            "status": status,
            "begin_at": begin_at,
            "end_at": end_at,
            "results": results
        }
        match_dict["winner"] = parse_opponent(match["winner"])
        match_dict["teams"] = [parse_opponent(
            opp["opponent"]) for opp in match["opponents"]]

        if match_dict["winner"] is not None:
            del match_dict["winner"]["image_url"]
        parsed_data.append(match_dict)

    with open(output_file, 'w') as f:
        json.dump(parsed_data, f)


if __name__ == "__main__":
    app()
