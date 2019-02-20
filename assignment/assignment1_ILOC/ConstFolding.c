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
#include "InstrUtils.h"
#include "Utils.h"

int main()
{
	Instruction *head;
  Instruction *ptr;
  Instruction *origin_ptr;
  
  int keep_field1;
  int keep_field3;  

	head = ReadInstructionList(stdin);
 
	if (!head) {
		WARNING("No instructions\n");
		exit(EXIT_FAILURE);
	}

	/* YOUR CODE GOES HERE */
  ptr = head;
  
  while (ptr!=NULL){
    if(ptr->opcode == LOADI){
      origin_ptr = ptr->prev;
      keep_field1 = ptr->field1; // constant
      
      if(ptr->next->opcode == LOADI){
        ptr = ptr->next;
        
        if (ptr->next->opcode == ADD){
          keep_field1 = keep_field1 + (ptr->field1); //add constant
          keep_field3 = ptr->next->field3; // keep the register number of ADD op code
          
          Instruction *newopcode = (Instruction *) malloc (sizeof (Instruction));
          newopcode -> opcode = LOADI;
          newopcode -> field1 = keep_field1;
          newopcode -> field2 = keep_field3;
          
          origin_ptr -> next = newopcode;
          newopcode -> next = ptr-> next ->next;
          
          free(ptr->prev);
          free(ptr->next);
          free(ptr);
          
          ptr= newopcode;
       } else if (ptr->next->opcode == SUB){
         keep_field1 = keep_field1 - (ptr->field1); //add constant
         keep_field3 = ptr->next->field3; // keep the register number of SUB op code
          
         Instruction *newopcode = (Instruction *) malloc (sizeof (Instruction));
         newopcode -> opcode = LOADI;
         newopcode -> field1 = keep_field1;
         newopcode -> field2 = keep_field3;
          
         origin_ptr -> next = newopcode;
         newopcode -> next = ptr-> next ->next;
         
         free(ptr->prev);
         free(ptr->next);
         free(ptr);
          
         ptr= newopcode;
      } else if (ptr->next->opcode == MUL){
         keep_field1 = keep_field1 * (ptr->field1); //add constant
         keep_field3 = ptr->next->field3; // keep the register number of SUB op code
          
         Instruction *newopcode = (Instruction *) malloc (sizeof (Instruction));
         newopcode -> opcode = LOADI;
         newopcode -> field1 = keep_field1;
         newopcode -> field2 = keep_field3;
          
         origin_ptr -> next = newopcode;
         newopcode -> next = ptr-> next ->next;
         
         free(ptr->prev);
         free(ptr->next);
         free(ptr);
          
         ptr= newopcode;
      } else {
        ptr = ptr->prev;
      }
    }
   } 
    ptr=ptr->next;
 }
 

	if (head) 
		PrintInstructionList(stdout, head);
	
	return EXIT_SUCCESS;
}

