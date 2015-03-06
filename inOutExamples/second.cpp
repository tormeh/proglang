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



///////////////////////compile with:    clang++ -std=c++11 -Weverything -lpthread first.cpp && ./a.out && rm a.out


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
static std::mutex mainSyncMutex;
static std::condition_variable cv;
static std::condition_variable cvmain;

static void waitForRendezvous(std::string name)
{
  std::unique_lock<std::mutex> lk(rendezvousSyncMutex);
  ++rendezvousCounter;
  {
    std::lock_guard<std::mutex> lk(mainSyncMutex);
    cvmain.notify_all();
  }
  std::cout << name << " rendezvousCounter(b): " << rendezvousCounter.load() << "\n";
  std::cout << name << " entering wait\n";
  cv.wait(lk);
  std::cout << name << " released from wait\n";
}

static void threadPrintHello(std::list<std::string>* println, int syncd)
{
  std::cout << "hello call\n";
  println->push_back("Hello");
  std::cout << "hello rendezvousCounter(a): " << rendezvousCounter.load() << "\n";
  waitForRendezvous("hello");
  threadPrintHello(println, syncd);
}

static void threadPrintWorld(std::list<std::string>* println, int syncd)
{
  std::cout << "world call\n";
  println->push_back(" world\n");
  std::cout << "world rendezvousCounter(a): " << rendezvousCounter.load() << "\n";
  waitForRendezvous("world");
  threadPrintWorld(println, syncd);
}


int main()
{
  //std::cout << "Hello World!\n";
  rendezvousCounter.store(0);
  int synchronizedNumber = 0;
  std::list<std::string> out1;
  std::list<std::string> out2;
  std::thread t1 (threadPrintHello, &out1, synchronizedNumber);
  std::thread t2 (threadPrintWorld, &out2, synchronizedNumber);
  while(true)
  {
    /*std::chrono::seconds sec(1);
    std::this_thread::sleep_for(sec);*/
    {
      std::unique_lock<std::mutex> lk(mainSyncMutex);
      cvmain.wait(lk);
    }
    std::cout << "main  rendezvousCounter: " << rendezvousCounter.load() << "\n";
    if(rendezvousCounter.load() >= NUMTOPTHREADS)
    {
      std::cout << "main  out1, out2 is size " << out1.size() << "," << out2.size() << "\n";
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
        std::cout << "main  notifying all\n";
        std::lock_guard<std::mutex> lk(rendezvousSyncMutex);
        cv.notify_all();
      }
      //std::this_thread::sleep_for(sec);
      std::cout << "main  notified all\n";
    }
    
  }
  //t1.join();
  //t2.join();
}
