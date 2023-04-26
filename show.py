#!/usr/bin/env python3
import subprocess
from pathlib import Path
from datetime import timedelta, datetime
import argparse


def main():
    parser = argparse.ArgumentParser(description="show latest packages updates")
    parser.add_argument(
        "limit", type=str, help="time limit like 3d for 3 days and 1w for one weeks"
    )
    args = parser.parse_args()
    limit = None
    if args.limit[-1] == "d":
        limit = timedelta(days=int(args.limit[:-1]))
    elif args.limit[-1] == "w":
        limit = timedelta(weeks=int(args.limit[:-1]))
    else:
        print("bad time limit")
        exit(1)

    now = datetime.now()
    since = now - limit
    root = Path(__file__).parent
    repos = root / "straight" / "repos"
    lastest_and_repos = []
    for repo in repos.iterdir():
        ts = subprocess.check_output(
            "git show -s --format=%ct", cwd=repo, shell=True
        ).rstrip()
        ts = datetime.fromtimestamp(int(ts))
        if ts > since:
            lastest_and_repos.append((ts, repo))

    lastest_and_repos.sort(reverse=True)
    for ts, repo in lastest_and_repos:
        print(f"\033[35m{repo.name}\033[0m")
        output = subprocess.check_output(
            [
                "git",
                "-c",
                "color.ui=always",
                "log",
                "--pretty=format:%Cred%h%Creset %s %Cgreen(%cr)",
                "--abbrev-commit",
                f"--since={int(since.timestamp())}",
            ],
            cwd=repo,
        )
        print(output.decode("utf-8"))


if __name__ == "__main__":
    main()
