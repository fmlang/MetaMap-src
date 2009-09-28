package wsd.util;

import java.io.*;

/**
 * Integer Binary Search Map
 *<p>
 * organization of one record:
 * <pre>
 *  +------------------------+-------------------+
 *  | term                   |      data         |
 *  +------------------------+-------------------+
 *  |<---- term length ----->|<---- 4 bytes ---->|
 *  |<------------- record length -------------->|
 * </pre>
 *
 *  Term Length And Data Length Is The Same For All Records In Map.
 * </p>
 * Created: Wed Jul 25 09:09:18 2001
 *
 * @author <a href="mailto:wrogers@nlm.nih.gov">Willie Rogers</a>
 * @version $Id: IntBinSearchMap.java,v 1.1 2006/09/20 20:47:11 wrogers Exp $
 */

public class IntBinSearchMap implements BinSearchMap, Serializable {

  /** length of integer in bytes */
  public static final int DATALENGTH = 4; /* is this right? */
  /** data output stream for writing map. */
  private transient DataOutputStream mapWriter;
  /** random access file for reading map. */
  private transient RandomAccessFile mapRAFile;
  /** number of records in this map. */
  int numberOfRecords = 0;
  /** term length of all terms in this map. */
  private int termLength = 0;
  /** canonical name of class*/
  public static final String canonicalSerializedName = "IntBinSearchMap.ser";
  /** filename of map */
  String filename;

  /**
   * Instantiate a new or existing binary search map with single integers for data.
   * @param mapFilename filename of map
   * @param mode        file mode to use: utils.IntBinSearchMap.WRITE to open map for writing, and
   *                    utils.IntBinSearchMap.READ to open map for reading.
   */
  public IntBinSearchMap ( String mapFilename, int mode )
    throws FileNotFoundException
  {
    if ( mode == WRITE ) {
      this.mapWriter = 
	new DataOutputStream ( new BufferedOutputStream
			       ( new FileOutputStream ( mapFilename )));
    } else {
      this.mapRAFile = new RandomAccessFile ( mapFilename, "r");
    }
    this.filename = mapFilename;
  }

  public static IntBinSearchMap getInstance(String filename)
    throws FileNotFoundException, IOException, ClassNotFoundException
  {
    StringBuffer strbuf = new StringBuffer();
    strbuf.append(filename).append("_").append(canonicalSerializedName);
    String serializedInfo =  strbuf.toString();
    if ( new File(serializedInfo).exists() ) {
      // System.out.println(" loading " + serializedInfo);
      FileInputStream istream = new FileInputStream(serializedInfo);
      ObjectInputStream p = new ObjectInputStream(istream);
      IntBinSearchMap index = (IntBinSearchMap)p.readObject();
      istream.close();
      index.mapRAFile = new RandomAccessFile ( filename, "r");
      index.filename = filename;
      return index;
    } else {
      System.err.println("  " + serializedInfo);
    }
    return null;
  }

  /**
   * Write an entry into map.
   * @param term term 
   * @param data data to be assoicated with term
   */
  public void writeEntry(String term, int data)
    throws IOException
  {
    // write dictionary entry
    this.mapWriter.writeBytes(term);
    this.mapWriter.writeInt(data);
    this.numberOfRecords++;
    this.termLength = term.length();
  }

  /**
   * get data entry for term 
   * @param term term 
   * @return int value associated with term
   */
  public int get(String term)
    throws IOException
  {
    if (this.mapRAFile == null ) {
       this.mapRAFile = new RandomAccessFile ( this.filename, "r");
    }
    return DiskBinarySearch.intBinarySearch(this.mapRAFile, 
					    term, term.length(), this.numberOfRecords);
  }

  /** 
   * @return get number of records in map
   */
  public int getNumberOfRecords()
  {
    return this.numberOfRecords;
  }

  /**
   * @return get length of data in each record
   */
  public int getDataLength()
  {
    return this.DATALENGTH;
  }

  /** close resources used by this map. */
  public void close()
    throws IOException
  {
    if (this.mapRAFile != null ) {
      this.mapRAFile.close();
    }
    if (this.mapWriter != null ) {
      this.mapWriter.close();
    }
  }

  public void serializeMapInfo()
    throws FileNotFoundException, IOException
  {
    /* serialize info on object to indexDirectoryPath/<Canonical Serialized Name> */
    FileOutputStream ostream = 
      new FileOutputStream(this.filename + "_" + canonicalSerializedName);
    ObjectOutputStream p = new ObjectOutputStream(ostream);
    p.writeObject(this);
    p.flush();
    p.close();
    ostream.close();
  }
}// IntBinSearchMap
