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

package wsd.util;

import java.net.Socket;
import java.io.PrintWriter;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

/**
 * This class represents an object that consists of a Socket object,  and BufferedReader /
 * PrintWriter objects associated with the Socket. A SocketResource is used by a
 * SocketResourcePool, where a certain number of sockets (and associated reader/writer
 * objects) are kept. The objects that need a socket pulls a SocketResource object
 * from the pool, uses it and releases it back to the pool.
 *
 * <P>This code was developed for National Library of Medicine, Cognitive
 * Science Branch.
 *
 * <p>Description: Word Sense Disambiguation</p>
 *
 * @version  04/02/02
 * @author   Halil Kilicoglu
 */


public class SocketResource
{
  /** A Socket */
  private Socket fSocket;
  /** The PrintWriter associated with the Socket */
  private PrintWriter fWriter;
  /** The BufferedReader associated with the Socket */
  private BufferedReader fReader;

  /**
   * Constructor. Creates a Socket object and its associated Reader/Writer objects.
   *
   * @param host the name of the host the SocketResource will open to
   * @param port the port number on the host
   *
   * @throws IOException if Reader/Writer objects cannot created.
   */
  public SocketResource(String host, int port) throws IOException
  {
      fSocket = new Socket(host,port);
      fWriter = new PrintWriter(fSocket.getOutputStream(), true);
      fReader = new BufferedReader(new InputStreamReader(fSocket.getInputStream()));
  }

  /**
   * get() method for the Socket.
   *
   * @return fSocket.
   */
  public Socket getSocket()
  {
      return fSocket;
  }

  /**
   * get() method for the PrintWriter.
   *
   * @return fWriter.
   */
  public PrintWriter getWriter()
  {
      return fWriter;
  }

  /**
   * get() method for BufferedReader.
   *
   * @return fReader.
   */
  public BufferedReader getReader()
  {
      return fReader;
  }

  /**
   * Checks whether a SocketResource is usable for socket operations.
   * A SocketResource object is usable if the Socket is not closed, and the Reader/Writer
   * objects are alive.
   *
   * @return true/false depending on whether the SocketResource is usable or not.
   */
  public boolean isSocketResourceOK()
  {
      if (fSocket.isClosed() || fSocket.isInputShutdown() || fSocket.isOutputShutdown())
          return false;
      return true;
  }

}