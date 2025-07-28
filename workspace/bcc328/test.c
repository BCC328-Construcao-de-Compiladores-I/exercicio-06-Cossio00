#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char* concat(const char* s1, const char* s2) {
  size_t len1 = strlen(s1);
  size_t len2 = strlen(s2);
  char* result = malloc(len1 + len2 + 1);
  strcpy(result, s1);
  strcat(result, s2);
  return result;
}

char* itoa(int value, char* str, int base) {
  snprintf(str, 32, "%d", value);
  return str;
}

int main() {
  {
    int Var "x" = 1;
  int Var "a" = (Var "x" + 1);
  printf("%d\n", Var "a");
  {
    int Var "x" = 5;
  int Var "a" = (Var "x" + 1);
  printf("%d\n", Var "a");
  }
  printf("%d\n", Var "a");
  }
  return 0;
}