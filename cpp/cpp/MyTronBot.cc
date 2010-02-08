// MyTronBot.cc
//
// This is the code file that you will modify in order to create your entry.

#include "Map.h"
#include <string>
#include <vector>

#define CHOICES 4194304
unsigned char * choices = null;

#define NORTH 0x0
#define EAST  0x1
#define SOUTH 0x2
#define WEST  0x3
#define DIRMASK 0x3

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
CHOICE choiceOf( bool * around ) {

}
DIRECTION getChoice(CHOICE n) {
	DIRMASK & ( choices[ n >> 4 ] >>(n & DIRMASK) );
}

// Ignore this function. It is just handling boring stuff for you, like
// communicating with the Tron tournament engine.
int main() {
  choices = malloc(CHOICES);
  memset(choices, 0, CHOICES);
  while (true) {
    Map map;
    Map::MakeMove(MakeMove(map));
  }
  return 0;
}
