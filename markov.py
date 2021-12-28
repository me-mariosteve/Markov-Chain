import random



def dict_sum(d: dict):
    """
    The same as `sum(d.values())`
    """
    
    return sum(d.values())


def calc_probs(s: str):
    """
    Calculates the probabilty of each characters to be found after a specific character.
    """
    
    out = dict()
    for last, c in zip(s[:-1], s[1:]):
        if (last, c) in out:
            out[last, c] += 1
        else:
            out[last, c] = 1
    return out
    

def calc_file_probs(filename: str, encoding: str = 'utf-8'):
    """
    Finds the probability of each pair to appear in the file `filename`.
    """

    probs = {}

    with open(filename, 'r', encoding=encoding) as file:

        last = None
        while True:
            c = file.read(1)

            if c == '': # EOF
                return probs

            elif (last, c) in probs and last:
                probs[last, c] += 1

            else:
                probs[last, c] = 1
            
            last = c


def next_char(_probs: dict[tuple[str], int], last: str) -> str:
    """
    Calculate a random character with the last character and the given probability for each character to appear.
    """
    
    # Gets the probabilities for the patterns starting with the character `last`.
    probs = {
        key: val
        for key, val in _probs.items()
        if key[0] == last
    }
    
    total = dict_sum(probs)
    rnd = random.randint(0, total)
    
    counter = 0
    for key, val in probs.items():
        counter += val
        if counter >= rnd:
            return key[1]
    return ""


def generate(probs: dict[tuple[str], int], length: int) -> str:
    """
    Generate a random string of length `length` by using the probabilities `probs` of each pattern to appear.
    """
    
    if probs == {}:
        return ""

    # To get the first character of the string, which is the first of a random pattern.
    out: str = random.choice(list(probs.keys()))[0]
    
    for i in range(length):
        n = next_char(probs, out[-1])
        if n:
            out += n
        else:
            return out
    
    return out
