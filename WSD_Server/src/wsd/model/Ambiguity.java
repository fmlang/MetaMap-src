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

import java.util.List;
import java.util.Vector;

import org.jdom.Element;
import org.jdom.Namespace;

/**
 * This class represents an Ambiguity object that consists of a list of candidates,
 * one of which corresponds to the actual meaning of the word in that context.
 *
 * <P>This code was developed for National Library of Medicine, Cognitive
 * Science Branch.
 *
 * <p>Description: Word Sense Disambiguation</p>
 *
 * @version  04/02/02
 * @author   Halil Kilicoglu
 */

public class Ambiguity
{
  /** whether this particular ambiguity need to be handled */
  private boolean fNeedProcessing = true;
  /** a list of candidates that cause an ambiguity instance */
  private List fCandidates;

  /**
   * Default constructor for the class. No parameter.
   */
  public Ambiguity()
  {
  }

  /**
   * Constructor. Sets the member fields when the object is created.
   *
   * @param   needProcessing  whether the Ambiguity need to be processed when it's going
   *                          through a Disambiguation method.
   * @param   candidates      a List of Candidate objects associated with the ambiguity.
   */
  public Ambiguity(boolean needProcessing,
                   List candidates)
  {
      fNeedProcessing = needProcessing;
      fCandidates = candidates;
  }

  /**
   * Constructor. Creates an Ambiguity object from a XML tree node. Candidate
   * list is created empty in this case. Use setCandidates() to set it later.
   *
   * @param node  the Ambiguity node in the XML tree.
   * @param ns    default namespace.
   */
  public Ambiguity(Element node, Namespace ns)
  {
      String process = node.getAttributeValue("process",ns);
      if (process.equals("no"))
        fNeedProcessing = false;
      fCandidates = new Vector();
  }

  /**
   * set() method for need_processing. need_processing tells us whether that
   * ambiguity is marked to be processed by the Machine Output Parser.
   *
   * @param   needProcessing whether an ambiguity should be handled.
   */
  public void setNeedProcessing(boolean needProcessing)
  {
      fNeedProcessing = needProcessing;
  }

  /**
   * set() method for the candidates.
   *
   * @param   candidates    the candidate list of the ambiguity
   */
  public void setCandidates(List candidates)
  {
      fCandidates = candidates;
  }

  /**
   * get() method for fNeedProcessing .
   *
   * @return  whether this ambiguity is marked up for processing.
   */
  public boolean getNeedProcessing()
  {
      return fNeedProcessing;
  }

  /**
   * get() method for the candidates associated with the ambiguity.
   *
   * @return  the candidate list.
   */
  public List getCandidates()
  {
      return fCandidates;
  }

  /**
   * Creates an easy-to-read string representation of the ambiguity. The format
   * is as follows: <I>[will_process|[candidate1|candidate2|..]]</I>.
   * Used mostly for logging/debugging purposes.
   *
   * @return  the string representation of the ambiguity.
   */
  public String toString()
  {
      StringBuffer buf = new StringBuffer();

      buf.append("[");
      buf.append(fNeedProcessing);
      buf.append("|");

      for (int i = 0; i < fCandidates.size(); i++)
      {
          buf.append(((Candidate)fCandidates.get(i)).toString());
          if (i < fCandidates.size() -1)
          {
              buf.append("|");
          }
      }
      buf.append("]");
      return buf.toString().trim();
  }
}
