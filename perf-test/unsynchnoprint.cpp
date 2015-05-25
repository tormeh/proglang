//clang++ original.cpp -pthread -std=c++11 -O3 -march=native && time ./a.out

#include <iostream>
#include <thread>
#include <string>
#include <chrono>


[[noreturn]] static void threadPrintWorld();
[[noreturn]] static void threadPrintHello();
[[noreturn]] static void threadPrintLol();
void actionPrintLol$threadPrintLol();
int integerIdentity$(int x);
void actionPrintFoo$threadPrintLol();
void actionPrintFooo$threadPrintLolactionPrintFoo();

static int synchronizedNumber = 0;



[[noreturn]] static void threadPrintWorld()
{while(true)
{
  continue;
}
}

[[noreturn]] static void threadPrintHello()
{while(true)
{
  synchronizedNumber = (synchronizedNumber + 1);
  if(synchronizedNumber >= 20000)
  {
    exit(0);
  }
  continue;
}
}

[[noreturn]] static void threadPrintLol()
{while(true)
{
  actionPrintLol$threadPrintLol();
  actionPrintFoo$threadPrintLol();

  continue;
}
}

void actionPrintLol$threadPrintLol()
{

}

int integerIdentity$(int x)
{
  return x;
}

void actionPrintFoo$threadPrintLol()
{
  actionPrintFooo$threadPrintLolactionPrintFoo();
}

void actionPrintFooo$threadPrintLolactionPrintFoo()
{

}


int main()
{
std::thread tthreadPrintHello (threadPrintHello);
std::thread tthreadPrintWorld (threadPrintWorld);
std::thread tthreadPrintLol (threadPrintLol);
while(true)
{
std::this_thread::sleep_for(std::chrono::seconds(1));
}
}
