#include <Adafruit_MAX31865.h>

// Use software SPI: CS, DI, DO, CLK
Adafruit_MAX31865 thermo = Adafruit_MAX31865(10, 11, 12, 13);
// use hardware SPI, just pass in the CS pin
// Adafruit_MAX31865 thermo = Adafruit_MAX31865(10);


void setup() {
  Serial.begin(9600);
  thermo.begin(MAX31865_2WIRE);  // set to 2WIRE or 4WIRE as necessary
}


void loop() {
  unsigned long clock = millis();
  uint8_t fault = thermo.readFault();
  float temp = thermo.temperature(100.0,430.0);

  Serial.print(clock);
  Serial.print(",");
  Serial.print(temp);
  Serial.write(13);
  Serial.write(10);

  if (fault) {
    thermo.clearFault();
  }

  delay(200);
}
