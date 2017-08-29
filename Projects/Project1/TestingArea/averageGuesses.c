/* 
   Proj1.hs Average Guess Calculator Program

   Calculates average number of guesses for 1330 target chords,
   for an initial target chord. 

   To use this program:

   1. Modify 'Proj1Test.hs' such that it only prints the total number of guesses

   2. In Bash, pipe all guess numbers into a file 'allAnswers'

      sh allTest.sh > allAnswers

   3. Compile this program

      gcc -Wall -o averageGuesses averageGuesses.c

   4. Pipe 'allAnswers' file into this program, and it will output the average
      number of guesses.

      ./averageGuesses < allAnswers


   Made by Emmanuel Macario.

*/

#include <stdio.h>
#include <stdlib.h>

#define MAX_FILENAME_LEN 25
#define MAX_TARGETS 1330


int main(int argc, char *argv[]) {
    FILE *fp;


    /* Open handle to file, and check it opened successfully. */
    fp = fopen("allAnswers", "r");
    if (!fp) {
        exit(EXIT_FAILURE);
    }

    int n=0, count = 0, total_guesses = 0;
    double avg_guesses;

    /* Keep reading in guesses, until there are no more,
       incrementing the total number of guesses. */
    while (fscanf(fp, "%d\n", &n) != EOF) {
        total_guesses += n;
        count++;
    }

    /* Now, check if we have tested ALL the possible target chords
       for a given guess. */
    if (count != MAX_TARGETS) {
        printf("Note: Not all 1,330 test cases were ran for this trial.");
    }

    /* Print the number of tests. */
    printf("Number of tests : %3d\n", count);

    /* Compute the average number of guesses, and print the result. */
    avg_guesses = (1.0 * total_guesses) / count;
    printf("Average guesses: %.3f\n", avg_guesses);

    
    /* Job done! */
    fclose(fp);

    return 0;
 }




    


