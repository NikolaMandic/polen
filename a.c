#include "stdlib.h"
#define width 5
#define height 5
#define size width*height
char x[size] = {0,1,0,0,0,0
               ,1,0,0,2,0,0
               ,1,0,0,2,0,0
               ,1,0,0,2,0,0
               ,1,0,0,2,0,0};

int cur;

int f(char c ){
  char r=x[cur*width+c];
  return r;
}

int main(int argc,void * argv){

  int i=0;
  int r=0;
  char *xx="asdasd";
  while (i<strlen(xx)){
    printf("strlen %d i=%d\n",strlen(xx),i);
    r=f(x[i]-'a');
    if (r>0) break
    printf("r=%d",r);
    i++;
  }
  printf("x");
  return 0;
}
