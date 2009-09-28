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
//    06/27/06    Willie Rogers    Initial Version
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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringReader;

import com.sleepycat.db.Db;
import com.sleepycat.db.DbException;

import gov.nih.nlm.nls.utils.StringUtils;

/**
 * Berkeley DB Loader for Susanne's Semantic Type Indexing tables.
 * 
 * <P>This code was developed for National Library of Medicine, Cognitive
 * Science Branch.  Adapted from Halil's MeSHFrequencyCalculator.java
 * database loader.
 *
 * <p>$Id</p>
 *
 * <p>Description: Word Sense Disambiguation</p>
 *
 * @version  27jun2006
 * @author   Willie Rogers
 */

public class StiPopulateDb
 {

  public StiPopulateDb(String inputFile, String outputFile)
  {
    createStiBtree(inputFile, outputFile);
  }

  private void createStiBtree(String inputFile, String outputFile)
  {
    String line;
    int err;
    try {
      new File(outputFile).delete();
      Db table = new Db(null, 0);
      table.set_error_stream(System.err);
      table.set_flags(Db.DB_DUPSORT);
      table.set_errpfx("StiPopulateDb");
      table.open(null, outputFile, null, Db.DB_BTREE, Db.DB_CREATE, 0644);
      BufferedReader in = new BufferedReader(new FileReader(new File(inputFile)));
      int i = 0;
      while ((line = in.readLine()) != null) {
        if ((i % 1000) == 0) System.out.print(i + "*");
             // System.out.println("Processing " + line);
          String term = StringUtils.getToken(line, " ", 0);
          if (term != null) {
            StringDbt key = new StringDbt(term);
            StringDbt data = new StringDbt(line);
            if ((err = table.put(null, key, data, 0)) != 0) {
              System.out.println("Problem with adding the key/data: " + term + "+" + line);
              System.out.println("Error code: " + err);
            }
          }
          i++;
        }
      table.close(0);
    } catch (DbException dbe) {
        System.err.println("StiPopulateDb->createStiBtree()::DbException: " + dbe.toString());
    } catch (FileNotFoundException fnfe) {
        System.err.println("StiPopulateDb->createStiBtree()::FileNotFoundException: " + fnfe.getMessage());
    } catch (IOException ioe) {
        System.err.println("StiPopulateDb->createStiBtree()::IOException: " + ioe.getMessage());
    }
  }

  public final static void main(String[] args)
  {
    if (args.length > 1) {
      new StiPopulateDb(args[0],args[1]);
    } else {
      System.err.println("usage: java wsd.util.StiPopulateDb tablefile dbfile");
      System.exit(1);
    }
  }
}
