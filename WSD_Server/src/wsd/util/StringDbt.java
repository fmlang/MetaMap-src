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

import com.sleepycat.db.Dbt;
import com.sleepycat.db.Db;

/**
 * This class is used for convenience in creating key and data elements when
 * the b-tree representations are created or data is retrieved from the
 * b-tree. BerkeleyDB API methods use byte arrays to communicate with database.
 * In order to simplify byte array-string conversions and flag setting procedures,
 * this class was extended from Dbt class.
 *
 * <P>This code was developed for National Library of Medicine, Cognitive
 * Science Branch.
 *
 * <p>Description: Word Sense Disambiguation</p>
 *
 * @version  04/02/02
 * @author   Halil Kilicoglu
 */
public class StringDbt extends Dbt {

  /**
   * Default constructor. No parameters.
   */
  public StringDbt()
  {
      set_flags(Db.DB_DBT_MALLOC); // tell Db to allocate on retrieval
  }

  /**
   * Constructor that uses a byte-array.
   *
   * @param arr byte-array used to create a StringDbt object.
   */
  public StringDbt(byte[] arr)
  {
      set_flags(Db.DB_DBT_USERMEM);
      set_data(arr);
      set_size(arr.length);
  }

  /**
   * Constructor that uses a String.
   *
   * @param value the string used to create a StringDbt object.
   */
  public StringDbt(String value)
  {
      setString(value);
      set_flags(Db.DB_DBT_MALLOC); // tell Db to allocate on retrieval
  }

  /**
   * Creates a database key or data field from a string.
   *
   * @param value a string that will be used to create a key or data value.
   */
  public void setString(String value)
  {
      set_data(value.getBytes());
      set_size(value.length());
      // must set ulen because sometimes a string is returned
      set_ulen(value.length());
  }
  /**
   * Returns the string representation of a key or data value.
   *
   * @return  the string representation of the data of a Dbt object.
   */
  public String getString()
  {
      return new String(get_data(), 0, get_size());
  }
}