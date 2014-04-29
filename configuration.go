package configuration

import (
   "log"
   proto "code.google.com/p/goprotobuf/proto"
   zmq   "github.com/vaughan0/go-zmq"
)

func configuration_server(connect_to string) {
   ctx, err := zmq.NewContext()
   if err != nil {
      log.Fatal("Could not create a ZMQ context.");
      panic(err)
   }
   defer ctx.Close()

   sock, err := ctx.Socket(zmq.Sub)
   if err != nil {
     log.Fatal("Could not create a socket.");
     panic(err)
   }
   defer sock.Close()

   if err = sock.Connect(connect_to); err != nil {
      log.Fatal("Could not connect to " + connect_to);
      panic(err)
   }

   chans := sock.Channels()
   defer chans.Close()

   for {
     select {
       case msgs := <-chans.In():
           go func() {
               for _, msg := range msgs {
                  request := new(Request);
                  err := proto.Unmarshal(msg, request);
                  if err != nil {
                     log.Print("Unable to unmarshal a configuration request.");
                  }
               }
           }()

       case err := <-chans.Errors():
           panic(err)
     }
   }
}

func Start(listen string) {
   go configuration_server(listen);
}

