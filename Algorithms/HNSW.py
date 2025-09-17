import copy

import numpy as np
import pandas as pd


def assign_neighbours(data: pd.DataFrame) -> pd.DataFrame:
    data_with_neighbours = copy.copy(data)
    for level in range(top_level):
        level_data = data[data["level"] <= level]
        for _, node in level_data.iterrows():
            node["neighbours"][level] = np.random.randint(0, n_points, M)

    return data_with_neighbours


def find_nearest_neighbours(index: int, data: pd.DataFrame, level=0, n_neighbours=1):
    x, y = data.loc[index, "x"], data.loc[index, "y"]
    search_data = copy.copy(data[data["level"] <= level])
    search_data["distance"] = (search_data["x"] - x) ** 2 + (search_data["y"] - y) ** 2
    top_entries = search_data.sort_values(by="distance", ascending=True).iloc[
        :n_neighbours
    ]
    nearest_neighbours = [n.name for _, n in top_entries.iterrows()]

    return nearest_neighbours


def greedy_walk(start_index, data: pd.DataFrame, level: int) -> int:
    index = start_index
    walk_indices = [start_index]
    while True:
        neighbours = data.loc[index, "neighbours"][level]
        nns = find_nearest_neighbours(index, data.loc[neighbours + [index]])


# Sample from 2D Gaussian
M = 5
n_points = 500
points = np.random.randn(n_points, 2)

# Assign layers according to geometric probability
level_probability_decay = 0.5
levels = np.random.geometric(level_probability_decay, size=n_points) - 1
top_level = max(levels)

# Package data
data = assign_neighbours(
    pd.DataFrame(
        {
            "x": [p[0] for p in points],
            "y": [p[1] for p in points],
            "level": levels,
            "neighbours": [dict() for _ in range(n_points)],
        }
    )
)

# Algorithm
greedy_walk(0, data, 0)
