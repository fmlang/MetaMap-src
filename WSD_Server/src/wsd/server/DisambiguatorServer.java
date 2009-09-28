//=== File Prolog ==========================================================
//    This code was developed for National Library of Medicine, Cognitive
//    Science Branch
//
//--- Notes ----------------------------------------------------------------
//
//
//--- Development History --------------------------------------------------
//    Date        Author             Reference
//    ----        ------             ---------
//    04/02/02    Halil Kilicoglu    Initial Version
//
//--- Warning --------------------------------------------------------------
//    This software is property of the National Library of Medicine.
//    Unauthorized use or duplication of this software is
//    strictly prohibited.  Authorized users are subject to the following
//    restrictions:
//    *   Neither the author, their corporation, nor NLM is responsible for
//        any consequence of the use of this software.
//    *   The origin of this software must not be misrepresented either by
//        explicit claim or omission.
//    *   Altered versions of this software must be plainly marked as such.
//    *   This notice may not be removed or altered.
//
//=== End File Prolog ======================================================

package wsd.server;

import java.io.IOException;
import java.net.ServerSocket;

import wsd.WSDEnvironment;

/**
 * The main WSD Server class. Receives disambiguation requests from the client
 * over a socket, creates a thread to handle the request and returns the results
 * back to the client.
 *
 * <P>This code was developed for National Library of Medicine, Cognitive
 * Science Branch.
 *
 * <p>Description: Word Sense Disambiguation</p>
 *
 * @version  04/02/02
 * @author   Halil Kilicoglu
 */
public class DisambiguatorServer
{
  /** whether the WSD server is currently listening */
  private boolean fListening = true;
  /** the port on which the WSD Server is listening */
  private int fPort;
  /** the server socket associated with the WSD Server */
  private ServerSocket fServerSocket = null;

  /**
   * The WSD Server constructor. Creates a server process on the port specified.
   *
   * @param port the server port.
   */
  private DisambiguatorServer(int port)
  {
      fPort = port;
      try
      {
          fServerSocket = new ServerSocket(fPort);
      }
      catch (IOException ioe)
      {
	  System.out.println("Could not listen on port : " + fPort + " : " + ioe.getMessage() );
	  fListening = false;
          System.exit(1);
      }
  }

  /**
   * The main() method for the WSD Server. Initializes the WSD Environment,
   * creates a server process and waits for the incoming disambiguation requests.
   */
  public static void main(String[] args)
  {
      // the server program takes no argument
      if (args.length > 0)
      {
          usage();
      }
      else
      {
          try
          {
              WSDEnvironment.initialize();
              DisambiguatorServer server = new DisambiguatorServer(WSDEnvironment.fPort);
              while (server.fListening)
                  new DisambiguatorServerThread(server.fServerSocket.accept()).start();
              server.fServerSocket.close();
              WSDEnvironment.shutdown();

          }
          catch (IOException ioe)
          {
              System.err.print("DisambiguatorServer->main()::IOException: " + ioe.getMessage());
          }
          catch (Exception e)
          {
              System.err.println("Server cannot be started.");
	      e.printStackTrace(System.err);
          }
      }
      System.exit(0);
  }

  /**
   * If the program is run with wrong options etc., this method is called.
   */
  private static void usage()
  {
      System.out.println("Usage: java wsd.DisambiguatorServer");
      System.out.println("Cannot start the WSD Server");
  }
}

