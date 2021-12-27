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
    

def next_char(_probs: dict[tuple[str], int], last: str):
    """
    Calculate a random character with the last character and the given probabilities for each character to appear.
    """
    
    # Gets the probabilities for the patterns starting with the character `last`.
    probs = {
        key, val
        for key, val in probs.items()
        if key[0] == last
    }
    
    total = dict_sum(probs)
    rnd = random.randint(0, s)
    
    counter = 0
    for key, val in probs.items():
        counter += val
        if counter >= rnd:
            return key


def generate(probs: dict[tuple[str], int], length: int):
    """
    Generate a random string of length `length` by using the probabilities `probs` of each pattern to appear.
    """
    
    # To get the first character of the string, which is the first of a random pattern.
    out = random.choice(list(probs.keys()))[0]
    
    for i in range(length):
        out += next_char(probs, out[-1])
    
    return out
