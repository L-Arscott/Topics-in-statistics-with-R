# Decision tree implementation 03/06/2023
import mysql.connector
import pandas as pd
from math import log2

def _obtain_data(qual_threshold=7):
    # Obtain data
    with open('password.txt') as f:
        my_password = f.readline()

    # Create a connection to the MySQL database by specifying the host, user, password, and database name:
    mydb = mysql.connector.connect(
        host="localhost",
        user="root",
        password=my_password,
        database="my_database"
    )

    # Create a cursor object to execute SQL queries:
    mycursor = mydb.cursor()

    # SQL query
    query = '''
    SELECT *
    FROM winequality_red
    '''
    mycursor.execute(query)  # Issue SQL query
    myresult = mycursor.fetchall()  # Retrieve results

    # Create a pandas DataFrame from the list of tuples:
    column_names = [desc[0] for desc in mycursor.description]
    df = pd.DataFrame(myresult, columns=column_names)

    # Set quality to 0 or 1
    df['quality'] = df['quality'].apply(lambda x: 1 if x >= qual_threshold else 0)
    df.rename(columns={'quality': 'class'}, inplace=True)

    return df


##
class Node:
    """Node of a decision tree

    Parameters
    ----------
    df: pandas.core.frame.DataFrame
        Dataframe describing points assigned to the node.

    Attributes
    ----------
    df: pandas.core.frame.DataFrame
        Dataframe describing points assigned to the node.
    left: Node
        Left child node
    right: Node
        Right child node
    max_inf_col: str
        Column according to which a split will yield maximum information gain
    max_inf_val: float
        Value according to which a split will yield maximum information gain
    p: float
        Proportion of ones
    """

    def __init__(self, df):
        self.df = df
        self.left = None
        self.right = None
        self.max_inf_col = None
        self.max_inf_val = None
        self.p = sum(df['class']) / len(df)  # int

    def gen_children(self):
        # Generate child nodes
        self.max_inf_col, self.max_inf_val = self._choose_feature()

        # No children if choose feature returns none, i.e. minimum information gain not met
        if not self.max_inf_col:
            return None, None

        else:
            l, r = _split(self.df, self.max_inf_col, self.max_inf_val)
            return Node(l), Node(r)

    def _choose_feature(self, min_inf_gain=0.01):
        # Returns column and value according to which a split yields maximum information
        # Returns default values '' and 0 if min_inf_gain is not met.
        max_observed_inf_gain = 0
        max_inf_col = ''
        max_inf_val = 0

        # Split according to each possible value of each feature
        for column in self.df.drop('class', axis=1).columns:
            unique_vals = self.df[column].unique().tolist()
            for val in unique_vals:
                df_left, df_right = _split(self.df, column, val)

                # Skip the case where a df is empty after split
                if len(df_left) == 0 or len(df_right) == 0:
                    continue

                # Assess information gain
                prev_entropy = _entropy(self.df)
                post_entropy = (len(df_left) * _entropy(df_left) + len(df_right) * _entropy(df_right)) / len(self.df)

                inf_gain = prev_entropy - post_entropy

                if inf_gain > max_observed_inf_gain and inf_gain > min_inf_gain:
                    max_observed_inf_gain = inf_gain
                    max_inf_col = column
                    max_inf_val = val

        return max_inf_col, max_inf_val


class Tree:
    """Decision tree

    Parameters
    ----------
    df: pandas.core.frame.DataFrame
        Dataframe describing points assigned to the node.

    Attributes
    ----------
    df: pandas.core.frame.DataFrame
        Dataframe describing points assigned to the node.

    Methods
    -------
    gen
        Generates the tree
    print
        Prints all nodes and their depth
    predict_entry
        Given a dataframe entry, ives the proportion of ones in the corresponding leaf
    predict_df
        Performs above for entire dataframe. Returns a dataframe column.
    test_predict_df
        Test function, see function for details
    """
    def __init__(self, df):
        self.df = df
        self.root = Node(df)

    def gen(self, max_depth=5):
        queue = [(self.root, 1)]

        while queue:
            leaf, depth = queue.pop(0)
            l, r = None, None

            if depth != max_depth:
                l, r = leaf.gen_children()

            if l is not None:
                leaf.left, leaf.right = l, r
                queue += [(l, depth + 1), (r, depth + 1)]

    def print(self):
        queue = [(self.root, 1)]

        while queue:
            leaf, depth = queue.pop(0)
            print(leaf.p, depth)
            l, r = leaf.left, leaf.right

            if l is not None:
                queue.append((l, depth + 1))
                queue.append((r, depth + 1))

    def predict_entry(self, entry):
        import copy
        leaf = copy.deepcopy(self.root)
        while True:
            if not leaf.max_inf_col:
                return leaf.p

            elif entry[leaf.max_inf_col] >= leaf.max_inf_val:
                leaf = leaf.left

            else:
                leaf = leaf.right

    def predict_df(self, df):
        predictions = df.apply(lambda row: self.predict_entry(row), axis=1)
        return predictions

    def test_predict_df(self):
        # Tests predict_df by running it on training data
        # Asserts exact proportions are assigned to each leaf

        self.df['predictions'] = self.predict_df(self.df)
        check = self.df.groupby('predictions')
        for p, df_p in check:
            assert (sum(df_p['class']) / len(df_p) == p)
        print('Success')


def _entropy(df, mode="Gini"):
    # Returns the entropy of a binary state
    # Options for Shannon entropy, Gini entropy
    p = sum(df['class']) / len(df)  # proportions of ones

    if mode == "Gini":
        ent = 1 - p**2 - (1-p)**2

    elif mode == "Shannon":
        if p == 1 or p == 0:
            return 0

        ent = -p * log2(p) - (1-p) * log2(1-p)  # Shannon entropy

    else:
        print('Unknown mode')
        return ValueError

    return ent

def _split(df, column, val):
    # Splits dataframe according to column and val
    df_left = df[df[column] >= val]
    df_right = df[df[column] < val]

    return df_left, df_right


##
our_df = _obtain_data()
test_tree = Tree(our_df)
test_tree.gen()
test_tree.print()
our_df['predictions'] = test_tree.predict_df(our_df)
print('done')

##
test_tree.test_predict_df()
