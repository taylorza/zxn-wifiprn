# ZXN Wifi Printer Driver
PoC for a simple wifi printer driver that enables direct printing to most modern printers from your ZX Spectrum Next.

## Assembling

To assemble the sample application use the following `sjasmplus` command line

```
sjasmplus src/wifiprn_drv.asm --inc=inc --msg=war --fullpath
```

## Installing the driver

1. Copy the driver `wifiprn.drv` to the SD Card
2. Using either the BASIC editor or the Command Line run the following command
```
.install wifiprn.drv
```
3. Tell the driver what the IP address is of the printer you want to print to on your network by opening the driver on the standard printer stream and providing the IP address. In the example below, replace the IP address with the IP of your printer.
```
OPEN #3, "d>P>192.168.5.215"
```
4. Test your printer by trying the following BASIC command
```
LPRINT "Hello World"
```

**Note** You need to have the ZX Spectrum Next connected to your network for the driver to work.
