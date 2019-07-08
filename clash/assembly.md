# Bill of Materials (BOM)

<img src="https://github.com/reactive-systems/KitchenTimer/blob/master/clash/assembly/bom.jpg" alt="BOM" width="600">

| Quantity | Description | Specification |
|:-:|:-|:-|
| 1  | iCEblink40HX1K Evaluation Kit | [Homepage](https://www.latticesemi.com/en/Products/DevelopmentBoardsAndKits/iCEblink40HX1KEvaluationKit) |
| 1  | Kitchen Timer PCB | [Gerber Files](https://github.com/reactive-systems/KitchenTimer/blob/master/clash/assembly/gerber) |
| 1  | PNP Transistor | [BC547C](https://github.com/reactive-systems/KitchenTimer/blob/master/clash/assembly/datasheets/bc547c) |
| 4  | 7-Segment Display | [SA36-11GWA](https://github.com/reactive-systems/KitchenTimer/blob/master/clash/assembly/datasheets/7segment.pdf) |
| 1  | Buzzer | active or passive, RM 6.5mm |
| 3  | Push Button Switch | DIP THT, 4-Pin, 6mm |
| 4  | 2.7kΩ Resistor | SMD, 0805 type |
| 28 | 56Ω Resistor | SMD, 0805 type |
| 4  | Pin-Header   | male, RM 2.54mm, 1 x 6 |
| 2  | Pin-Header | male, RM 2.54mm, 2 x 6 |
| 3  | Pin-Header | male, RM 2.54mm, 2 x 8 |

# Assembly Instructions

We start with the bottom side of the Kitchen Timer PCB.

<img src="https://github.com/reactive-systems/KitchenTimer/blob/master/clash/assembly/step0.jpg" alt="BOM" height="300">

First, solder the four 2.7kΩ SMD resistors to the PCB (R8, R9, R10, R32).

<img src="https://github.com/reactive-systems/KitchenTimer/blob/master/clash/assembly/step1.jpg" alt="BOM" height="300">

Afterwards, the remaining 28 56Ω SMD resistors are soldered to the PCB.

<img src="https://github.com/reactive-systems/KitchenTimer/blob/master/clash/assembly/step2.jpg" alt="BOM" height="300">

We continue with the four 7-segment displays. Take care that each segment has the right orientation.

<img src="https://github.com/reactive-systems/KitchenTimer/blob/master/clash/assembly/step3a.jpg" alt="BOM" height="300">&nbsp;&nbsp;<img src="https://github.com/reactive-systems/KitchenTimer/blob/master/clash/assembly/step3b.jpg" alt="BOM" height="300">

The next components are the buzzer, the tansistor and the three push bottons.

<img src="https://github.com/reactive-systems/KitchenTimer/blob/master/clash/assembly/step4a.jpg" alt="BOM" height="300">&nbsp;&nbsp;<img src="https://github.com/reactive-systems/KitchenTimer/blob/master/clash/assembly/step4b.jpg" alt="BOM" height="300">

For soldering the male pin headers to the PBC you can use the iCEblink board as a support. To this end, first place all male pin headers in their female counterpart.

<img src="https://github.com/reactive-systems/KitchenTimer/blob/master/clash/assembly/step5.jpg" alt="BOM" height="300">

Now, it is easy to align the Kitchen Timer PCB on top and solder everything together.

<img src="https://github.com/reactive-systems/KitchenTimer/blob/master/clash/assembly/step6a.jpg" alt="BOM" height="300">&nbsp;&nbsp;<img src="https://github.com/reactive-systems/KitchenTimer/blob/master/clash/assembly/step6b.jpg" alt="BOM" height="300">

Finally, put the shield on the iceblink board and upload the syntheized sketch.

<img src="https://github.com/reactive-systems/KitchenTimer/blob/master/clash/application.png" alt="Final Setup" height="300">
