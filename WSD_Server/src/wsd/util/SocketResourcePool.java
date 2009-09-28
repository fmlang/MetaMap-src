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

import java.util.Iterator;
import java.util.Vector;

import org.apache.log4j.Logger;

import wsd.WSDEnvironment;

/**
 * This class represents a Singleton object that handles initialization, allocation
 * and cleanup of a number of SocketResource objects that handle communication with
 * JDI Server. It is started up during application initialization.
 * <p>  The SocketResourcePool can be generalized to support connection to multiple
 * servers using a factory method with a singleton collection of SocketResourcePools.
 *
 * <P>This code was developed for National Library of Medicine, Cognitive
 * Science Branch.
 *
 * <p>Description: Word Sense Disambiguation</p>
 *
 * @version  04/02/02
 * @author   Halil Kilicoglu
 */

public class SocketResourcePool
{
  /** The Singleton instance */
  private static SocketResourcePool fSocketResourcePool = null;
  /** The SocketResourceFactory instance that creates the SocketResource's */
  private static SocketResourceFactory fSocketResourceFactory = null;
  /** The SocketResource objects that are available */
  private Vector freeSocketResources = null;
  /** the SocketResource objects that are currently being used */
  private Vector usedSocketResources = null;
  /** # of SocketResource's in the pool */
  private int POOL_SIZE = WSDEnvironment.fSocketResourcePoolSize;
  /** # of broken SocketResources */
  private int brokenSockets = 0;

  private static Logger logger = Logger.getLogger(SocketResourcePool.class);

  /**
   * Constructor. Initializes the Pool.
   */
  private SocketResourcePool()
  {
      init();
  }

  /**
   * If a Pool doesn't exist, creates one. Otherwise, returns the existing instance.
   *
   * @return SocketResourcePool instance.
   */
  public static synchronized SocketResourcePool getInstance()
  {
      if (fSocketResourcePool == null)
      {
         fSocketResourcePool = new SocketResourcePool();
      }
      return fSocketResourcePool;
  }

   /**
    * Creates a number of SocketResource's and places them in freeSocketResources list.
    */
  private void init()
  {
      int i=0;
      fSocketResourceFactory = SocketResourceFactory.getInstance();
      freeSocketResources = new Vector(POOL_SIZE);
      usedSocketResources = new Vector(POOL_SIZE);
      for (i=0; i< POOL_SIZE; i++)
      {
          freeSocketResources.add(createSocketResource());
      }
      logger.info("Created " + POOL_SIZE + " JDI Sockets.");
  }

  /**
   * Creates a SocketResource to JD Server.
   *
   * @return a SocketResource to JD Server.
   */
  private SocketResource createSocketResource()
  {
      try
      {
          SocketResource socketResource =
              fSocketResourceFactory.newSocketResource(WSDEnvironment.fJdServerHostName,
                                                       WSDEnvironment.fJdServerPort);
          return socketResource;
      }
      catch (IOException ioe)
      {
          logger.error(ioe.getMessage());
          return null;
      }
  }

  /**
   * Checks to find a SocketResource that is available for use. If none of them
   * are available, waits for one to become available.
   *
   * @return a SocketResource that is available and working.
   */
  public synchronized SocketResource getSocketResource()
  {
      SocketResource socketResource = null;
      boolean broken = true;
      if (freeSocketResources.size() > 0)
      {
          if (logger.isDebugEnabled())
          {
              logger.debug("Getting socket resource: Number of available sockets: " + freeSocketResources.size());
              logger.debug("Getting socket resource: Number of used sockets: " + usedSocketResources.size());
          }
          while (broken)
          {
              socketResource = (SocketResource)freeSocketResources.remove(0);
              if (!socketResource.isSocketResourceOK())
                  releaseBrokenSocketResource(socketResource);
              else
                  broken = false;
          }
          usedSocketResources.add(socketResource);
          if (logger.isDebugEnabled())
          {
              logger.debug("Got socket resource: Number of free sockets: " + freeSocketResources.size());
              logger.debug("Got socket resource: Number of used sockets: " + usedSocketResources.size());
          }
          return socketResource;
      }
      else
      {
          try
          {
              wait();
          }
          catch (InterruptedException ie)
          {
              logger.error("Cannot fetch a socket." + ie.getMessage());
          }
           return getSocketResource();
      }
  }

  /**
   * Releases a used SocketResource and returns it to freeSocketResources list.
   * Notifies the processes that are waiting for a SocketResource to become
   * available.
   *
   * @param socketResource  the SocketResource object to be released.
   */
  public synchronized void releaseSocketResource(SocketResource socketResource)
  {
      int index = usedSocketResources.indexOf(socketResource);
      if (index != -1) {
          freeSocketResources.add(usedSocketResources.remove(index));
      }
      if (logger.isDebugEnabled())
      {
          logger.debug("Release: Number of free sockets: " + freeSocketResources.size());
          logger.debug("Release: Number of used sockets: " + usedSocketResources.size());
      }
      notifyAll();
  }

  /**
   * Releases a broken SocketResource object. Removes the SocketResource and
   * creates a new one, which is then added to freeSocketResources list.
   *
   * @param socketResource  a broken SocketResource to be released.
   */
  public synchronized void releaseBrokenSocketResource(SocketResource socketResource)
  {
    try
    {
          brokenSockets++;
          if (logger.isDebugEnabled())
              logger.debug("# of broken sockets: " + brokenSockets);
          socketResource.getWriter().flush();
          socketResource.getWriter().close();
          socketResource.getReader().close();
          socketResource.getSocket().close();
          usedSocketResources.remove(usedSocketResources.indexOf(socketResource));
          SocketResource newSocketResource = createSocketResource();
          freeSocketResources.add(newSocketResource);
          brokenSockets--;
    }
    catch (IOException ioe)
    {
        logger.error("Problem with releasing broken socket. " + ioe.getMessage());
    }
  }

  /**
   * Removes all existing SocketResource objects.
   *
   * @throws IOException when one of the close() or flush() operations fails.
   */
  public synchronized void shutdown() throws IOException
  {
      Iterator iterator = freeSocketResources.iterator();
      while (iterator.hasNext())
      {
          SocketResource socketResource = (SocketResource)iterator.next();
          socketResource.getWriter().flush();
          socketResource.getWriter().close();
          socketResource.getReader().close();
          socketResource.getSocket().close();
      }
      iterator = usedSocketResources.iterator();
      while (iterator.hasNext())
      {
          SocketResource socketResource = (SocketResource)iterator.next();
          socketResource.getWriter().flush();
          socketResource.getWriter().close();
          socketResource.getReader().close();
          socketResource.getSocket().close();
      }

  }

  /**
   * Reports free SocketResource object count in the Pool.
   *
   * @return the # of free SocketResource's.
   */
  public int getFreeSocketResourceCount()
  {
      return freeSocketResources.size();
  }

  /**
   * Reportns currently used SocketResource object count in the Pool.
   *
   * @return the # of currently used SocketResource's.
   */
  public int getUsedSocketResourceCount()
  {
      return usedSocketResources.size();
  }

  /**
   * Reports # of broken SocketResources.
   *
   * @return the # of broken SocketResource's.
   */
  public int getBrokenSocketResourceCount()
  {
      return brokenSockets;
  }
}
