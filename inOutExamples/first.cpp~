/*

program p:Nothing =
{
  synchronized variable synchronizedNumber:Integer = {0}
  threadPrintHello(synchronizedNumber)
  threadPrintWorld(synchronizedNumber)
}
  
thread threadPrintHello(in:Integer):Nothing =
{
  println("hello ")
  threadPrintHello()
}

thread threadPrintWorld(in:Integer):Nothing =
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
static std::atomic<int> atomictestCounter;
static std::mutex rendezvousSyncMutex;
static std::condition_variable cv;

static void waitForRendezvous(std::string name)
{
  std::unique_lock<std::mutex> lk(rendezvousSyncMutex);
  ++rendezvousCounter;
  cv.wait(lk);
}

static void threadPrintHello(std::list<std::string>* println, int syncd, int it)
{
  while(true)
  {
    println->push_back("Hello");
    waitForRendezvous("hello");
  }
}

static void threadPrintWorld(std::list<std::string>* println, int syncd, int it)
{
  while(true)
  {
    println->push_back(" world\n");
    waitForRendezvous("world");
  }
}


int main()
{
  //std::cout << "Hello World!\n";
  rendezvousCounter.store(0);
  int synchronizedNumber = 0;
  std::list<std::string> out1;
  std::list<std::string> out2;
  std::thread t1 (threadPrintHello, &out1, synchronizedNumber, 0);
  std::thread t2 (threadPrintWorld, &out2, synchronizedNumber, 0);
  while(true)
  {
    if(rendezvousCounter.load() >= NUMTOPTHREADS)
    {
      while(!out1.empty())
      {
        std::cout << out1.front();
        out1.pop_front();
      }
      while(!out2.empty())
      {
        std::cout << out2.front();
        out2.pop_front();
      }
      {
        rendezvousCounter.store(0);
        std::lock_guard<std::mutex> lk(rendezvousSyncMutex);
        cv.notify_all();
      }
    }
    
  }
}
