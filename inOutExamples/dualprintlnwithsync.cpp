/*

program p:Nothing =
{
  synchronized variable synchronizedNumber:Integer = {synchronized(variable=0, writer=threadPrintHello)}
  threadPrintHello(synchronizedNumber)
  threadPrintWorld(synchronizedNumber)
}

thread threadPrintWorld(synchronizedNumber:Integer):Nothing =
{
  println("world ")
  println(synchronizedNumber)
  println("\n")
  threadPrintWorld()
}
  
thread threadPrintHello(synchronizedNumber:Integer):Nothing =
{
  println(synchronizedNumber)
  println(" Hello ")
  mutate(variable=synchronizedNumber, newValue=plus(left=synchronizedNumber, right=1))
  threadPrintHello()
}

*/



///////////////////////compile with:    clang++ -O3 -std=c++11 -Weverything -pthread dualprintlnwithsync.cpp && ./a.out && rm a.out
/////////////////////////////////or:    g++ -O3 -std=c++11 -Wall -pthread dualprintlnwithsync.cpp && ./a.out && rm a.out


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
static std::list<std::string> print1;
static std::list<std::string> print2;
static int synchronizedNumber = 0;
static int writeSynchronizedNumber = 0;

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
    while(!print1.empty())
    {
      std::cout << print1.front();
      print1.pop_front();
    }
    while(!print2.empty())
    {
      std::cout << print2.front();
      print2.pop_front();
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

[[noreturn]] static void threadPrintHello()
{
  while(true)
  {
    print1.push_back(std::to_string(writeSynchronizedNumber));
    print1.push_back(" Hello ");
    writeSynchronizedNumber = writeSynchronizedNumber + 1;
    waitForRendezvous("hello");
  }
}

[[noreturn]] static void threadPrintWorld()
{
  while(true)
  {
    print2.push_back("world ");
    print2.push_back(std::to_string(synchronizedNumber));
    print2.push_back("\n");
    waitForRendezvous("world");
  }
}


int main()
{
  rendezvousCounter.store(0);
  std::thread t1 (threadPrintHello);
  std::thread t2 (threadPrintWorld);
  while(true)
  {
    std::this_thread::sleep_for(std::chrono::seconds(1));
  }
}
