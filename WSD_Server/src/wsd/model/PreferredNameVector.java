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

package wsd.model;

import java.util.Collection;
import java.util.Vector;
import java.util.StringTokenizer;

import wsd.WSDEnvironment;

/**
 * This class extends Vector class and provides an easy way to
 * convert a preferred name concept array to a string and vice versa. The string
 * format of a PreferredNameVector is used extensively throughout the WSD system.
 *
 * <P>This code was developed for National Library of Medicine, Cognitive
 * Science Branch.
 *
 * <p>Description: Word Sense Disambiguation</p>
 *
 * @version  04/02/02
 * @author   Halil Kilicoglu
 */
public class PreferredNameVector extends Vector {

  /**
   * Default constructor. No parameters.
   */
  public PreferredNameVector()
  {
      super();
  }

  /**
   * Constructor that creates a PreferredNameVector object from a Collection object.
   *
   * @param c the Collection object that forms the basis of the PreferredNameVector
   *          object
   */
  public PreferredNameVector(Collection c)
  {
      super(c);
  }

  /**
   * Constructor that creates a PreferredNameVector object from a string. The
   * input string is expected to have one of the following formats:
   * <P> <i>[concept1$concept2$...]</i>
   * <P> or
   * <P> <i> concept1$concept2$......</i>
   * <P> where $ is the PreferredNameVector delimiter. It may be set to a different
   * character using WSD Server configuration file.
   *
   * @param text  the input string
   */
  public PreferredNameVector(String text)
  {
      super();
      String trimmedText = text;
      if (text.charAt(0) == '[' && text.charAt(text.length()-1) == ']')
        trimmedText = text.substring(1,text.length()-1);
      StringTokenizer tokenizer = new StringTokenizer(trimmedText,WSDEnvironment.fPreferredNameSeparator);

      while (tokenizer.hasMoreTokens())
      {
          add(tokenizer.nextToken());
      }
  }

  /**
   * Converts a PreferredNameVector object to String.
   * <P> Output has the format: <i>[concept1$concept2$ ...]</i>
   *
   * @return the string representation of the PreferredNameVector object.

   */
  public String convertToString()
  {
      StringBuffer buf = new StringBuffer();
      buf.append("[");
      if (this.size() > 0)
      {
          for (int i=0; i< size(); i++)
          {
             buf.append((String)elementAt(i));
             if (i < size() - 1)
                buf.append(WSDEnvironment.fPreferredNameSeparator);
          }
      }
      buf.append("]");
      return buf.toString().trim();
  }

}