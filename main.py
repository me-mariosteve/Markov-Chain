import sys

import markov


def main(argc, argv):
    
    if argc == 1:
        print("Usage: main.py OPTION [OPTIONS ...]")
        return 0
    
    elif argc == 2:
        option = argv[1]
        
        # Displays how to use this program.
        if option == "help":
            print(
                "Usage: main.py OPTION [OPTIONS ...]\n"
                "Options:\n"
                "  help        Displays the help about this program.\n"
                "  calc TEXT        Calculate the porbability for each pattern of two characters to appear in the text TEXT.\n"
                "  gen TEXT LENGTH        Generate a random text of length LENGTH by using the probability of each pair of character in the text TEXT."
            )
            return 0

        # Calculates the probability for each pattern to appear.
        elif option == "calc":
            if argc == 3:
                text = argv[2]
                probs = markov.calc_probs(text)
                print(probs)
                return 0
                
            else:
                print("Usage of the option 'calc': main.py calc TEXT")
                return 1
        
        elif option == "gen":
                if argc != 4:
                    print("Usage of the option 'gen': main.py gen TEXT LENGTH")
                    return 1
                
                else:
                    text, length = argv[2], int(argv[3])
                    probs = markov.calc_probs(text)
                    out = markov.gen(probs, length)
                    print(out)
                    return 0


if __name__ == "__main__":
    return main(sys.argc, sys.argv)
