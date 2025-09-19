import copy
import random

import numpy as np
import pandas as pd


def assign_neighbours(data: pd.DataFrame) -> pd.DataFrame:
    """
    Given a dataframe of nodes, assigns M neighbours to each node, at each level the node is
    present.
    This is done in a dictionary:
        {0: [12, 45, 23, 89, 76}, 1: [...], ...}

    Neighbour assignment method:
        Random

    :param data: Dataframe of nodes

    :return: Dataframe of ndoes with new "neighbours" column.
    """
    data_with_neighbours = copy.copy(data)
    for level in range(top_level + 1):
        level_data = data[data["level"] >= level]
        level_nodes = level_data.index.tolist()
        for _, node in level_data.iterrows():
            node["neighbours"][level] = random.sample(level_nodes, min(M, len(level_nodes)))

    return data_with_neighbours


def find_nearest_neighbours(index: int, data: pd.DataFrame, n_neighbours: int = 1):
    x, y = data.loc[index, "x"], data.loc[index, "y"]
    return find_nearest_to_target(x, y, data, n_neighbours)


def find_nearest_to_target(x: float, y: float, data: pd.DataFrame, n_neighbours=1) -> list[int]:
    """
    Given a dataframe of nodes, finds the index of the one nearest to the target.

    :param x: Target x coordinate
    :param y: Target y coordinate
    :param data: Dataframe of nodes
    :param n_neighbours: Return the :param n_neighbours: nearest points.

    :return: Nearest point(s) to target
    """
    search_data = copy.copy(data)
    search_data["distance"] = (search_data["x"] - x) ** 2 + (search_data["y"] - y) ** 2
    top_entries = search_data.sort_values(by="distance", ascending=True).iloc[:n_neighbours]
    nearest_neighbours = [n.name for _, n in top_entries.iterrows()]

    return nearest_neighbours


def greedy_walk(
    start_index: int, target_x: float, target_y: float, data: pd.DataFrame, level: int
) -> int:
    """
    Performs greedy walk towards target.

    :param start_index: Index of start position
    :param target_x: Target x coordinate
    :param target_y: Target y coordinate
    :param data: Dataframe of nodes
    :param level: Level at which the walk is occurring

    :return: Index of the final position
    """
    position = copy.copy(start_index)
    walk_indices = [start_index]
    while True:
        neighbours = data.loc[position, "neighbours"][level]
        nn = find_nearest_to_target(target_x, target_y, data.loc[neighbours + [position]])[0]

        if nn == position:
            print(walk_indices)
            return position

        walk_indices.append(nn)
        position = copy.copy(nn)


# Sample from 2D Gaussian
M = 5
n_points = 500
points = np.random.randn(n_points, 2).tolist()

# Assign layers according to geometric probability
level_probability_decay = 0.5
levels = [int(x) for x in np.random.geometric(level_probability_decay, size=n_points) - 1]
top_level = max(levels)

# Package data
our_data = assign_neighbours(
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
our_index = int(our_data[our_data["level"] == top_level].iloc[0].name)
for current_level in reversed(range(top_level + 1)):
    our_index = greedy_walk(our_index, target_x=1, target_y=1, data=our_data, level=current_level)

print(
    f"Final position: \n"
    f"index: {our_index}\n"
    f"position: {our_data.loc[our_index, 'x']}, {our_data.loc[our_index, 'y']}\n"
)
