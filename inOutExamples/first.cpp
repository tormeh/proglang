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

static std::atomic<int> rendezvousCounter;
static std::atomic<int> atomictestCounter;
static std::mutex rendezvousSyncMutex;
static std::condition_variable cv;

static void threadPrintHello(std::list<std::string> println, int syncd)
{
  std::cout << "iterateHello\n";
  std::unique_lock<std::mutex> lk(rendezvousSyncMutex);
  println.push_back("Hello");
  std::cout << "rendezvousCounter in world(a): " << rendezvousCounter.load() << "\n";
  ++rendezvousCounter;
  std::cout << "rendezvousCounter in world(b): " << rendezvousCounter.load() << "\n";
  cv.wait(lk);
  threadPrintHello(println, syncd);
}

static void threadPrintWorld(std::list<std::string> println, int syncd)
{
  std::cout << "iterateWorld\n";
  std::unique_lock<std::mutex> lk(rendezvousSyncMutex);
  println.push_back(" world\n");
  std::cout << "rendezvousCounter in world(a): " << rendezvousCounter.load() << "\n";
  ++rendezvousCounter;
  std::cout << "rendezvousCounter in world(b): " << rendezvousCounter.load() << "\n";
  cv.wait(lk);
  threadPrintWorld(println, syncd);
}

int main()
{
  //std::cout << "Hello World!\n";
  rendezvousCounter.store(0);
  int synchronizedNumber = 0;
  std::list<std::string> out1;
  std::list<std::string> out2;
  std::thread t1 (threadPrintHello, out1, synchronizedNumber);
  std::thread t2 (threadPrintWorld, out2, synchronizedNumber);
  while(true)
  {
    std::chrono::seconds sec(1);
    std::this_thread::sleep_for(sec);
    std::cout << "rendezvousCounter in main: " << rendezvousCounter.load() << "\n";
    if(rendezvousCounter.load() == 2)
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
      
      rendezvousCounter.store(0);
      cv.notify_all();
    }
    
  }
  //t1.join();
  //t2.join();
}
