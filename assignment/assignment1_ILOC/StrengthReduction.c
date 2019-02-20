/*
 *********************************************
 *  314 Principles of Programming Languages  *
 *  Fall 2017                                *
 *  Author: Uli                              *
 *  Student Version                          *
 *********************************************
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "InstrUtils.h"
#include "Utils.h"

int count_PowerOfTwo (unsigned int x)
{
  int count=0;
  while (((x % 2) == 0) && x > 1) {
   x = x/2;
   count++;
  } 
 
  return count;
}


int main()
{
	Instruction *head;
  Instruction *ptr;
  Instruction *origin_ptr;
  
  int count=0;

	head = ReadInstructionList(stdin);
	if (!head) {
		WARNING("No instructions\n");
		exit(EXIT_FAILURE);
	}

	/* YOUR CODE GOES HERE */
 
  ptr = head-> next;
  
  while (ptr!=NULL){
    if(ptr->opcode == LOADI){
      origin_ptr = ptr->prev;
      count = count_PowerOfTwo(ptr->field1);
      if (count!=0){
      
      /* keep the constant that can l or r shift - how many time to shift*/
        
        if (ptr->next->opcode == MUL){
          
          Instruction *newopcode = (Instruction *) malloc (sizeof (Instruction));
          newopcode -> opcode = LSHIFTI;
          newopcode -> field1 = ptr->next->field1;
          newopcode -> field2 = count;
          newopcode->field3 = ptr->next->field3;
          
          origin_ptr -> next = newopcode;
          newopcode -> next = ptr-> next ->next;

          free(ptr->next);
          free(ptr);
          
          ptr= newopcode;
          
       } else if (ptr->next->opcode == DIV){
         Instruction *newopcode = (Instruction *) malloc (sizeof (Instruction));
          newopcode -> opcode = RSHIFTI;
          newopcode -> field1 = ptr->next->field1;
          newopcode -> field2 = count;
          newopcode->field3 = ptr->next->field3;
          
          origin_ptr -> next = newopcode;
          newopcode -> next = ptr-> next ->next;

          free(ptr->next);
          free(ptr);
          
          ptr= newopcode;
         
      } 
      
     } // if count == 0
    
   } // if loadI fail 
    ptr=ptr->next;
 }

	if (head) 
		PrintInstructionList(stdout, head);
	
	return EXIT_SUCCESS;
}

