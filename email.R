emailR 

Flags:

"-f" Takes in file name parameters separated by spaces; if no file specified it reads from stdin

"-a" Will attach files, if not specifed it will read the files and concatenate them with new lines

"-s" Subject line flag

"-r" Takes in email recipient parameters separated by spaces

"-b" Email body flag

Sample run

emailR "-f ~/file_name1 ~/filename2" "-r recipent1 recipient 2" "-s Testing emailR" "-b hello world"
