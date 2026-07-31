import os
import subprocess
import pandas as pd


def run_r_script(team_name, season, sport):
    # Find the project directory
    script_dir = os.path.dirname(os.path.abspath(__file__))

    # Build file paths
    r_script = os.path.join(
        script_dir,
        "get_VB_data.R"
    )

    data_dir = os.path.join(
        script_dir,
        "data"
    )

    main_team_file = os.path.join(
        data_dir,
        "main_team_stats.csv"
    )

    conference_defense_file = os.path.join(
        data_dir,
        "conference_defensive_stats.csv"
    )

    # Create the data directory
    os.makedirs(
        data_dir,
        exist_ok=True
    )

    # Remove old output files
    for output_file in [main_team_file, conference_defense_file]:
        if os.path.exists(output_file):
            os.remove(output_file)

    # Confirm that the R script exists
    if not os.path.exists(r_script):
        print("R script not found:")
        print(r_script)
        return None

    print("Running R script...")
    print("R script:", r_script)
    print("Data directory:", data_dir)

    try:
        print("Using R file:")
        print(r_script)

        with open(r_script, "r") as file:
            r_contents = file.read()

        print("Old R version:","No opponent-total defensive rows were found" in r_contents
)
        result = subprocess.run(
            [
                "Rscript",
                r_script,
                team_name,
                str(season),
                sport,
                data_dir
            ],
            capture_output=True,
            text=True,
            check=False
        )

    except FileNotFoundError:
        print("Rscript could not be found.")
        print("Make sure R is installed and available in PATH.")
        return None

    # Always show the output produced by R
    if result.stdout.strip():
        print("\nR output:")
        print(result.stdout)

    if result.stderr.strip():
        print("\nR warnings or errors:")
        print(result.stderr)

    print("R exit code:", result.returncode)

    if result.returncode != 0:
        print("The R script ended with an error.")
        return None

    expected_files = [
        main_team_file,
        conference_defense_file
    ]

    for output_file in expected_files:
        if not os.path.isfile(output_file):
            print("R did not create the expected output file:")
            print(output_file)
            return None

        if os.path.getsize(output_file) == 0:
            print("R created an empty output file:")
            print(output_file)
            return None

    return main_team_file, conference_defense_file


def load_team_data(team_name, season, sport):
    output_files = run_r_script(
        team_name,
        season,
        sport
    )

    if output_files is None:
        return None, None

    main_team_file, conference_defense_file = output_files

    try:
        # Main team's overall offensive/team statistics
        main_team_stats = pd.read_csv(main_team_file)

        # Other conference teams' defensive profiles
        conference_defensive_stats = pd.read_csv(
            conference_defense_file
        )

    except pd.errors.EmptyDataError:
        print("One of the CSV files does not contain readable data.")
        return None, None

    except Exception as error:
        print("Python could not read the CSV files.")
        print(error)
        return None, None

    return main_team_stats, conference_defensive_stats


def get_team_defensive_stats(conference_defensive_stats, team_name):
    # Return one conference opponent's defensive statistics
    if conference_defensive_stats is None:
        return None

    if "Team" not in conference_defensive_stats.columns:
        print("The conference data does not contain a Team column.")
        return None

    team_defense = conference_defensive_stats[
        conference_defensive_stats["Team"].str.lower().str.strip()
        == team_name.lower().strip()
    ].copy()

    if team_defense.empty:
        print("No defensive statistics were found for:", team_name)
        return None

    return team_defense


def main():
    team_name = "Ramapo"
    season = 2025
    sport = "MVB"

    main_team_stats, conference_defensive_stats = load_team_data(
        team_name,
        season,
        sport
    )

    if main_team_stats is not None:
        print("\nRamapo Overall Team Statistics")
        print(main_team_stats)

    if conference_defensive_stats is not None:
        print("\nOther Conference Teams Defensive Statistics")
        print(conference_defensive_stats)


main()