CXX? = g++
CFLAGS = -Wall -O3 -fPIC

all: libsvm.so

svm.o: svm.cpp svm.h
	$(CXX) $(CFLAGS) -c svm.cpp

clean:
	rm -f *~ svm.o

libsvm.so: svm.o
	$(CXX) $(CFLAGS) -shared -o libsvm.so svm.o
