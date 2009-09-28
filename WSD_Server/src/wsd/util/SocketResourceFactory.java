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

import java.io.IOException;

/**
 * This class represents a Singleton object that creates sockets and associated
 * PrintWriter and BufferedReader objects upon request. It is started up during
 * application initialization.
 *
 * <P>This code was developed for National Library of Medicine, Cognitive
 * Science Branch.
 *
 * <p>Description: Word Sense Disambiguation</p>
 *
 * @version  04/02/02
 * @author   Halil Kilicoglu
 */

public class SocketResourceFactory {

  /** The only instance of the Factory object */
  private static SocketResourceFactory fSocketResourceFactory;

  /** Constructor. No parameter. */
  private SocketResourceFactory()
  {
  }

  /**
   * Creates an instance of the Factory object if it doesn't exist already.
   *
   * @return the SocketResourceFactory instance.
   */
  public static SocketResourceFactory getInstance()
  {
      if (fSocketResourceFactory == null)
      {
         fSocketResourceFactory = new SocketResourceFactory();
      }
      return fSocketResourceFactory;
  }

  /**
   * Creates a new SocketResource object.
   *
   * @param host the name of the host the SocketResource will open to
   * @param port the port number on the host
   *
   * @throws IOException if Reader/Writer objects cannot created.
   */
  public SocketResource newSocketResource(String hostname, int port) throws IOException
  {
      return new SocketResource(hostname, port);
  }
}