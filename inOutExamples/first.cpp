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
static std::condition_variable cv;


static void threadPrintHello(std::list<std::string>* println, int syncd)
{
  std::cout << "hello call\n";
  println->push_back("Hello");
  std::cout << "hello rendezvousCounter(a): " << rendezvousCounter.load() << "\n";
  
  std::unique_lock<std::mutex> lk1(rendezvousSyncMutex);
  ++rendezvousCounter;
  std::cout << "hello rendezvousCounter(b): " << rendezvousCounter.load() << "\n";
  std::cout << "hello entering wait\n";
  cv.wait(lk1);
  std::cout << "hello released from wait\n";
  threadPrintHello(println, syncd);
}

static void threadPrintWorld(std::list<std::string>* println, int syncd)
{
  std::cout << "world call\n";
  println->push_back(" world\n");
  std::cout << "world rendezvousCounter(a): " << rendezvousCounter.load() << "\n";
  
  std::unique_lock<std::mutex> lk2(rendezvousSyncMutex);
  ++rendezvousCounter;
  std::cout << "world rendezvousCounter(b): " << rendezvousCounter.load() << "\n";
  std::cout << "world entering wait\n";
  cv.wait(lk2);
  std::cout << "world released from wait\n";
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
    std::chrono::seconds sec(1);
    std::this_thread::sleep_for(sec);
    std::cout << "main  rendezvousCounter: " << rendezvousCounter.load() << "\n";
    if(rendezvousCounter.load() == 2)
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
      
      rendezvousCounter.store(0);
      std::cout << "main  notifying all\n";
      cv.notify_one();
      std::this_thread::sleep_for(sec);
      std::cout << "main  notified all\n";
    }
    
  }
  //t1.join();
  //t2.join();
}
