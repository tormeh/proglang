//clang++ original.cpp -pthread -std=c++11 -O3 -march=native && time ./a.out

#include <iostream>
#include <thread>
#include <string>
#include <atomic>
#include <condition_variable>
#include <list>
#include <chrono>


#define NUMTOPTHREADS 3

[[noreturn]] static void threadPrintWorld();
[[noreturn]] static void threadPrintHello();
[[noreturn]] static void threadPrintLol();
void actionPrintLol$threadPrintLol();
int integerIdentity$(int x);
void actionPrintFoo$threadPrintLol();
void actionPrintFooo$threadPrintLolactionPrintFoo();

static int synchronizedNumber = 0;
static int writeSynchronizedNumber = 0;
static std::list<std::string> printthreadPrintHello;
static std::list<std::string> printthreadPrintWorld;
static std::list<std::string> printthreadPrintLol;
static std::atomic<int> rendezvousCounter;
static std::mutex rendezvousSyncMutex;
static std::condition_variable cv;static void waitForRendezvous(std::string name)
{
  std::unique_lock<std::mutex> lk(rendezvousSyncMutex);
  ++rendezvousCounter;
  if(rendezvousCounter.load() < NUMTOPTHREADS)
  {
    cv.wait(lk);
  }
  else if (rendezvousCounter.load() == NUMTOPTHREADS)
  {
    while(!printthreadPrintHello.empty()){
std::cout << printthreadPrintHello.front();
printthreadPrintHello.pop_front();
}
while(!printthreadPrintWorld.empty()){
std::cout << printthreadPrintWorld.front();
printthreadPrintWorld.pop_front();
}
while(!printthreadPrintLol.empty()){
std::cout << printthreadPrintLol.front();
printthreadPrintLol.pop_front();
}
synchronizedNumber = writeSynchronizedNumber;

    {
      rendezvousCounter.store(0);
      cv.notify_all();
    }
  }
  else
  {
    std::cout << "error in wait for " << name << ". Rendezvouscounter out of bounds. RedezvousCounter = " << rendezvousCounter.load() << "\n";
    exit(0);
  }
}



[[noreturn]] static void threadPrintWorld()
{while(true)
{
  printthreadPrintWorld.push_back("world ");
  printthreadPrintWorld.push_back(std::to_string(synchronizedNumber));
  waitForRendezvous("threadPrintWorld");
  continue;
}
}

[[noreturn]] static void threadPrintHello()
{while(true)
{
  printthreadPrintHello.push_back(std::to_string(synchronizedNumber));
  printthreadPrintHello.push_back(" Hello ");
  writeSynchronizedNumber = (synchronizedNumber + 1);
  if(synchronizedNumber>=20000)
  {
    exit(0);
  }
  waitForRendezvous("threadPrintHello");
  continue;
}
}

[[noreturn]] static void threadPrintLol()
{while(true)
{
  actionPrintLol$threadPrintLol();
  actionPrintFoo$threadPrintLol();
  waitForRendezvous("threadPrintLol");

  continue;
}
}

void actionPrintLol$threadPrintLol()
{
  printthreadPrintLol.push_back("  LOL   ");
}

int integerIdentity$(int x)
{
  return x;
}

void actionPrintFoo$threadPrintLol()
{
  printthreadPrintLol.push_back("  FOO   ");
  actionPrintFooo$threadPrintLolactionPrintFoo();
  printthreadPrintLol.push_back(std::to_string(integerIdentity$(5)));
  printthreadPrintLol.push_back("  ");
  printthreadPrintLol.push_back(std::to_string(6));
  printthreadPrintLol.push_back("\n");
}

void actionPrintFooo$threadPrintLolactionPrintFoo()
{
  printthreadPrintLol.push_back("  FOOO  ");
}


int main()
{
rendezvousCounter.store(0);
std::thread tthreadPrintHello (threadPrintHello);
std::thread tthreadPrintWorld (threadPrintWorld);
std::thread tthreadPrintLol (threadPrintLol);
while(true)
{
std::this_thread::sleep_for(std::chrono::seconds(1));
}
}
