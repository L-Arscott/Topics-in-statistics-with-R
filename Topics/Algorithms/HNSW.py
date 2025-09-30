import copy
import random

import numpy as np
import pandas as pd


def assign_neighbours(data: pd.DataFrame, max_neighbours) -> pd.DataFrame:
    """
    Given a dataframe of nodes, assigns M neighbours to each node, at each level the node is
    present.
    This is done in a dictionary:
        {0: [12, 45, 23, 89, 76}, 1: [...], ...}

    Neighbour assignment method:
        Random

    :param data: Dataframe of nodes
    :param max_neighbours: Max number of neighbours per node

    :return: Dataframe of ndoes with new "neighbours" column.
    """
    top_level = get_top_level(data)
    data_with_neighbours = copy.copy(data)
    for level in range(top_level + 1):
        level_data = get_level_data(data, level=level)
        level_nodes = level_data.index.tolist()
        for _, node in level_data.iterrows():
            node["neighbours"][level] = random.sample(
                level_nodes, min(max_neighbours, len(level_nodes))
            )

    return data_with_neighbours


def find_nearest_neighbours(index: int, data: pd.DataFrame, n_neighbours: int = 1):
    x, y = data.loc[index, "x"], data.loc[index, "y"]
    return find_nearest_to_target(x, y, data, n_neighbours)


def find_nearest_to_target(
    x: float, y: float, data: pd.DataFrame, n_nearest=1
) -> list[int]:
    """
    Given a dataframe of nodes, finds the index of the one nearest to the target.

    :param x: Target x coordinate
    :param y: Target y coordinate
    :param data: Dataframe of nodes
    :param n_nearest: Return the :param n_neighbours: nearest points.

    :return: Nearest point(s) to target
    """
    search_data = copy.copy(data)
    search_data["distance"] = (search_data["x"] - x) ** 2 + (search_data["y"] - y) ** 2
    top_entries = search_data.sort_values(by="distance", ascending=True).iloc[
                  :n_nearest
    ]
    nearest_neighbours = [n.name for _, n in top_entries.iterrows()]

    return nearest_neighbours


def get_level_data(data: pd.DataFrame, level: int) -> pd.DataFrame:
    return copy.copy(data[data["level"] >= level])


def get_top_level(data: pd.DataFrame) -> int:
    return max(data["level"].tolist())


def greedy_walk(
    start_index: int, target_x: float, target_y: float, data: pd.DataFrame, level: int
) -> [int, list[int]]:
    """
    Performs greedy walk towards target.

    :param start_index: Index of start position
    :param target_x: Target x coordinate
    :param target_y: Target y coordinate
    :param data: Dataframe of nodes
    :param level: Level at which the walk is occurring

    :return: Index of the final position, List of indices visited
    """
    position = copy.copy(start_index)
    walk_indices = [start_index]
    while True:
        neighbours = data.loc[position, "neighbours"][level]
        nn = find_nearest_to_target(target_x, target_y, data.loc[neighbours + [position]])[0]

        if nn == position:
            return position, walk_indices

        walk_indices.append(nn)
        position = copy.copy(nn)


def run_hnsw(m=5, n_points=500, target_x = 1, target_y = 1) -> [pd.DataFrame, dict, int]:
    # Sample from 2D Gaussian
    points = np.random.randn(n_points, 2).tolist()

    # Assign layers according to geometric probability
    level_probability_decay = 0.5
    levels = [
        int(x) for x in np.random.geometric(level_probability_decay, size=n_points) - 1
    ]
    top_level = max(levels)

    # Package data
    data = assign_neighbours(
        data=pd.DataFrame(
            {
                "x": [p[0] for p in points],
                "y": [p[1] for p in points],
                "level": levels,
                "neighbours": [dict() for _ in range(n_points)],
            }
        ),
        max_neighbours=m
    )

    # Algorithm
    index = int(data[data["level"] == top_level].iloc[0].name)
    walk = dict()
    for current_level in reversed(range(top_level + 1)):
        index, level_walk = greedy_walk(
            index, target_x=target_x, target_y=target_y, data=data, level=current_level
        )
        walk[current_level] = level_walk

    return data, walk, index


if __name__ == "__main__":
    point_data, hnsw_walk, final_index = run_hnsw()

    print(
        f"Walk: \n"
        f"{hnsw_walk}\n"
        f"Final position: \n"
        f"index: {final_index}\n"
        f"position: {point_data.loc[final_index, 'x']}, {point_data.loc[final_index, 'y']}\n"
    )
