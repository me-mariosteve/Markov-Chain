#! /bin/env python

import sys

import markov



def main(argc: int, argv: tuple[str]):
    
    if argc == 1:
        print("""Usage: main.py OPTION [OPTIONS ...]
Use 'main.py help' for more informations."""
        )
        return 0
    
    elif argc > 1:
        command: str = argv[1]
        
        # Displays how to use this program.
        if command == "help":
            print("""Usage: main.py COMMAND [OPTIONS ...]

Commands:

help
        Displays the help about this program.
        
calc { param | file | input } [ARG]
        Calculate the probability for each pair of characters to appear in:
            - the string ARG if the second argument is 'param',
            - the file at path ARG if the second argument is 'file',
            - from the standard input if the second argument is 'input'.

gen { param | file | input } [ARG] LENGTH
        Generate a random text of length LENGTH by using the probability of each pair of character to appear in:
            - the string ARG if the second argument is 'param',
            - the file at path ARG if the second argument is 'file',
            - the input if the second argument is 'input'.
""")
            return 0

        # Calculates the probability for each pattern to appear.
        elif command == "calc":
            if argc >= 2:
                option = argv[2]

                if option == "param":
                    if argc >= 3:
                        probs = markov.calc_probs(argv[3])
                        print(probs)
                        return 0
                    else:
                        print("Usage of the command 'calc' with 'param': main.py calc param TEXT")
                        return 1
                
                elif option == "file":
                    if argc >= 3:
                        probs = markov.calc_file_probs(argv[3])
                        print(probs)
                        return 0
                    else:
                        print("Usage of the command calc with 'file': main.py calc file FILENAME")
                        return 1
                
                elif option == "input":
                    while True:
                        print(markov.calc_probs(input()))
                
            else:
                print("Usage of the command 'calc': main.py calc { param | file | input } [ARG]")
                return 1
        
        elif command == "gen":
            if argc >= 2:
                option = argv[2]

                if option == "param":
                    if argc >= 3:
                        text: str = argv[3]
                        length: int = int(argv[4])
                        probs = markov.calc_probs(text)
                        print(markov.generate(probs, length))
                        return 0
                    else:
                        print("Usage of the command 'gen' with 'param': main.py gen param TEXT LENGTH")
                        return 1
                
                elif option == "file":
                    if argc == 5:
                        filename: str = argv[3]
                        length: int = int(argv[4])
                        probs = markov.calc_file_probs(filename)
                        print(markov.generate(probs, length))
                        return 0
                    else:
                        print("Usage of the command 'gen' with 'file': main.py gen file FILENAME LENGTH")
                        return 1
                
                elif option == "input":
                    if argc == 4:
                        length: int = int(argv[3])
                        while True:
                            probs = markov.calc_probs(input())
                            print(markov.generate(probs, length))
                    else:
                        print("Usage of the command 'gen' with 'input': main.py gen input LENGTH")
                
            else:
                print("Usage of the command 'gen': main.py gen { param | file | input } [ARG] LENGTH")
                return 1
        
        else:
            print(f"""Unrecognized command: '{command}'
Use 'main.py help' for more informations.""")


if __name__ == "__main__":
    
    main(len(sys.argv), sys.argv)
