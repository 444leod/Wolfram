##
## EPITECH PROJECT, 2024
## Wolfram
## File description:
## Makefile
##

NAME = wolfram

SRC = ./src/main.c

OBJ = $(SRC:.asm=.o)

CC = gcc

CFLAGS = -Wall -Wextra -Werror -Wpedantic

all: $(NAME)

$(NAME):
	stack build
	cp $(shell stack path --local-install-root)/bin/Wolfram-exe $(NAME)

clean:
	stack clean

fclean: clean
	rm -f $(NAME)

re: fclean all

tests_run: all

run: all

.PHONY: all clean fclean re
.SILENT: run
