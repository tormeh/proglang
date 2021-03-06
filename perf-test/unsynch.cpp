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
  std::cout << "world ";
  std::cout << std::to_string(synchronizedNumber);
  continue;
}
}

[[noreturn]] static void threadPrintHello()
{while(true)
{
  std::cout << std::to_string(synchronizedNumber);
  std::cout << " Hello ";
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
  std::cout << "  LOL   ";
}

int integerIdentity$(int x)
{
  return x;
}

void actionPrintFoo$threadPrintLol()
{
  std::cout << "  FOO   ";
  actionPrintFooo$threadPrintLolactionPrintFoo();
  std::cout << std::to_string(integerIdentity$(5));
  std::cout << "  ";
  std::cout << std::to_string(6);
  std::cout << "\n";
}

void actionPrintFooo$threadPrintLolactionPrintFoo()
{
  std::cout << "  FOOO  ";
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
