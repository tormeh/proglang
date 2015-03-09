/*

program p:Nothing =
{
  threadPrintHello()
  threadPrintWorld()
}
  
thread threadPrintHello():Nothing =
{
  println("hello ")
  threadPrintHello()
}

thread threadPrintWorld():Nothing =
{
  println("world")
  threadPrintWorld()
}

*/



///////////////////////compile with:    clang++ -O3 -std=c++11 -Weverything -pthread first.cpp && ./a.out && rm a.out
/////////////////////////////////or:    g++ -O3 -std=c++11 -Wall -pthread first.cpp && ./a.out && rm a.out


#include <iostream>
#include <thread>
#include <string>
#include <atomic>
#include <condition_variable>
#include <list>
#include <chrono>

#define NUMTOPTHREADS 2

static std::atomic<int> rendezvousCounter;
static std::mutex rendezvousSyncMutex;
static std::condition_variable cv;
static std::list<std::string> println1;
static std::list<std::string> println2;

static void waitForRendezvous(std::string name)
{
  std::unique_lock<std::mutex> lk(rendezvousSyncMutex);
  ++rendezvousCounter;
  if(rendezvousCounter.load() < NUMTOPTHREADS)
  {
    cv.wait(lk);
  }
  else if (rendezvousCounter.load() == NUMTOPTHREADS)
  {
    while(!println1.empty())
    {
      std::cout << println1.front();
      println1.pop_front();
    }
    while(!println2.empty())
    {
      std::cout << println2.front();
      println2.pop_front();
    }
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

static void threadPrintHello(int syncd)
{
  while(true)
  {
    println1.push_back("Hello");
    waitForRendezvous("hello");
  }
}

static void threadPrintWorld(int syncd)
{
  while(true)
  {
    println2.push_back(" world\n");
    waitForRendezvous("world");
  }
}


int main()
{
  //std::cout << "Hello World!\n";
  rendezvousCounter.store(0);
  int synchronizedNumber = 0;
  std::thread t1 (threadPrintHello, synchronizedNumber);
  std::thread t2 (threadPrintWorld, synchronizedNumber);
  while(true)
  {
    std::this_thread::sleep_for(std::chrono::seconds(1));
  }
}
