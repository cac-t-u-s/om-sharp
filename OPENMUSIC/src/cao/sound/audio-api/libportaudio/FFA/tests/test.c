#include <complex.h>

/* Motto: "I sincerely wish C had decent macros :-)" */

/* sum_* functions below return the sum of a vector of the given type */

unsigned short sum_uchar(unsigned short length, unsigned char* sequence) {
  unsigned int sum=0.0;
  for (int i=0; i < length; i++)
    sum += *(sequence+i);
  return sum;
}

unsigned short sum_ushort(unsigned short length, unsigned short* sequence) {
  unsigned int sum=0.0;
  for (int i=0; i < length; i++)
    sum += *(sequence+i);
  return sum;
}

unsigned long sum_ulong(unsigned short length, unsigned long* sequence) {
  unsigned long sum=0.0;
  for (int i=0; i < length; i++)
    sum += *(sequence+i);
  return sum;
}

float sum_float(unsigned short length, float* sequence) {
  float sum=0.0;
  for (int i=0; i < length; i++)
    sum += *(sequence+i);
  return sum;
}
  
double sum_double(unsigned short length, double* sequence) {
  double sum=0.0;
  for (int i=0; i < length; i++)
    sum += *(sequence+i);
  return sum;
}

/* NOTE: the complex cases return real^2+imag^2, because CFFI doesn't
   handle C99 complex types yet. */
  
float sum_complex_float(unsigned short length, complex float* sequence) {
  complex float sum=0.0;
  for (int i=0; i < length; i++)
    sum += *(sequence+i);
  return creal(sum)*creal(sum)+cimag(sum)*cimag(sum);
}
  
double sum_complex_double(unsigned short length, complex double* sequence) {
  complex double sum=0.0;
  for (int i=0; i < length; i++)
    sum += *(sequence+i);
  return creal(sum)*creal(sum)+cimag(sum)*cimag(sum);
}
  
/* add1_* functios below add 1 (or 1+1i, for complex cases) to a
   vector of a given type */

void add1_uchar(unsigned short length, unsigned char* sequence) {
  for (int i=0; i < length; i++)
    (*(sequence+i))++;
}

void add1_ushort(unsigned short length, unsigned short* sequence) {
  for (int i=0; i < length; i++)
    (*(sequence+i))++;
}

void add1_ulong(unsigned short length, unsigned long* sequence) {
  for (int i=0; i < length; i++)
    (*(sequence+i))++;
}

void add1_float(unsigned short length, float* sequence) {
  for (int i=0; i < length; i++)
    (*(sequence+i)) += 1.0;
}

void add1_double(unsigned short length, double* sequence) {
  for (int i=0; i < length; i++)
    *(sequence+i) += 1.0;
}

void add1_complex_float(unsigned short length, complex float* sequence) {
  for (int i=0; i < length; i++)
    (*(sequence+i)) += 1.0+1.0*I;
}

void add1_complex_double(unsigned short length, complex double* sequence) {
  for (int i=0; i < length; i++)
    (*(sequence+i)) += 1.0+1.0*I;
}
