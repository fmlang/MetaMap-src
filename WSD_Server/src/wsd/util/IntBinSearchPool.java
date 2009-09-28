package wsd.util;

/**
 * A pool of Integer Binary Search Maps
 *
 * @version $Id: IntBinSearchPool.java,v 1.1 2006/09/20 20:48:35 wrogers Exp $
 */ 

import java.io.*;
import java.util.*;

public class IntBinSearchPool
{
  /** pool of IntBinSearchMaps */
  Map pool = new HashMap(20);

  String indexname;
  String indexDirectoryPath;

  public IntBinSearchPool(String indexDirectoryPath, String indexname) {
    this.indexDirectoryPath = indexDirectoryPath;
    this.indexname = indexname;
  }

  public String getPartitionId(String term)
  {
    String keyLength = Integer.toString(term.length());
    return this.indexname+keyLength;
  }

  public IntBinSearchMap getIntBinSearchMap(String partitionId)
    throws FileNotFoundException, IOException, ClassNotFoundException
  {
    return IntBinSearchMap.getInstance(this.indexDirectoryPath + File.separator + "partition_" + partitionId);
  }

  public int get(String term)
    throws FileNotFoundException, IOException, ClassNotFoundException
  {
    IntBinSearchMap ibsm = null;
    String partitionId = getPartitionId(term);
    if (pool.containsKey(partitionId)) {
      ibsm = (IntBinSearchMap)pool.get(partitionId);
    } else {
      ibsm = getIntBinSearchMap(partitionId);
      pool.put(partitionId,ibsm);
    }
    if (ibsm != null)
      return ibsm.get(term);
    else 
      return -1;
  }

  public void close()
    throws IOException
  {
    Iterator poolIter = pool.values().iterator();
    while (poolIter.hasNext()) {
      ((IntBinSearchMap)poolIter.next()).close();
    }
  }
}

