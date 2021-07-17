// Correct implementation of ThreadId# equality for GHC < 9.
long effectful_eq_thread(void *tso1, void *tso2)
{
  return tso1 == tso2;
}
