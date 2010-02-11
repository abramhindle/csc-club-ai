// MyTronBot.cc
//
// This is the code file that you will modify in order to create your entry.
// This was supposed to be a genetic one
#include "Map.h"
#include <string>
#include <vector>
#include <ctime>
#include <cstdlib>


#define CWIDTH 5
#define CHEIGHT 5
#define CBITS 24
#define CHOICES 4194304
#define COFFSET 2
unsigned char * choices = null;

#define NORTH 0x0
#define EAST  0x1
#define SOUTH 0x2
#define WEST  0x3
#define DIRMASK 0x3

typedef CHOICE unsigned int

std::string MakeMove(const Map& map) {
  int x = map.MyX();
  int y = map.MyY();
  if (!map.IsWall(x, y-1)) {
    return "NORTH";
  }
  if (!map.IsWall(x+1, y)) {
    return "EAST";
  }
  if (!map.IsWall(x, y+1)) {
    return "SOUTH";
  }
  if (!map.IsWall(x-1, y)) {
    return "WEST";
  }
  return "NORTH";
}

CHOICE choiceOf(const Map& map ) {
  int mx = map.MyX();
  int my = map.MyY();
  CHOICE choice = 0;
  int i = 0;
  for (int y = 0; y < CWIDTH; y++) {
    for (int x = 0; x < CHEIGHT; x++) {

      if (!(x == 0 && y == 0)) {
        if (map.IsWall(x - COFFSET, y - COFFSET)) {
          choice = choice | (0x1 << i);
        }
        i++;
     }
  }
  return choice;
}

DIRECTION getChoice(CHOICE n) {
  return (DIRMASK & ( choices[ n >> 4 ] >>( (n & DIRMASK) << 1)));
}
void setChoice(CHOICE n, bool v) {
  if (bool) {
    
  }
  return (DIRMASK & ( choices[ n >> 4 ] >>(n & DIRMASK) ));
}

 
void mutate(int onein) {
  for (int i = 0; i < 4194304; i++) {
    if (0==rand(onein)) {
      choices[i] = (unsigned char)( 0xFF & rand() )
    }
  }
}

void dumpChoice() {

}

void loadChoice() {

}

// Ignore this function. It is just handling boring stuff for you, like
// communicating with the Tron tournament engine.
int main() {
  srand(time(NULL));  
  choices = malloc(CHOICES);
  memset(choices, 0, CHOICES);
  while (true) {
    Map map;
    Map::MakeMove(MakeMove(map));
  }
  return 0;
}
