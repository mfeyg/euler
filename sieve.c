#include <stdlib.h>
#include <strings.h>

int *sieve(int N) {
  int *s = malloc(N*sizeof(int));
  memset(s, 0, N*sizeof(int));
  for (int i=2; i<=N; ++i) if (s[i-1]==0) for (int n=i+i; n<=N; n+=i) s[n-1]=i;
  *s = 1;
  return s;
}
