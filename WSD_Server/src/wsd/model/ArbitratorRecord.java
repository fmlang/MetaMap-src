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

/**
 * This class represents an ArbitratorRecord object that consists of a
 * disambiguation method name, a concept that was returned by that method and
 * the weight of the method. It is used by the Arbitrator to compute the scores.
 *
 * <P>This code was developed for National Library of Medicine, Cognitive
 * Science Branch.
 *
 * <p>Description: Word Sense Disambiguation</p>
 *
 * @version  04/02/02
 * @author   Halil Kilicoglu
 */
public class ArbitratorRecord {

  /** Disambiguation method name */
  private String fMethodName;
  /** Preferred name for the UMLS Concept */
  private String fPreferredConceptName;
  /** Weight associated with the disambiguation method */
  private double fWeight;

  /**
   * Creates an ArbitratorRecord object from a UMLS concept (preferred name),
   * a method name and method weight.
   *
   * @param methodName  a disambiguation method name
   * @param conceptName a UMLS concept(preferred name) returned by the disambiguation method
   * @param score       the weight of the method
   */
  public ArbitratorRecord(String methodName, String conceptName, double weight)
  {
      fMethodName = methodName;
      fPreferredConceptName = conceptName;
      fWeight = weight;
  }

  /**
   * set() method for disambiguation method name.
   *
   * @param methodName the method name to set.
   */
  public void setMethodName(String methodName)
  {
      fMethodName = methodName;
  }

  /**
   * set() method for the UMLS concept.
   *
   * @param conceptName the concept name to set.
   */
  public void setPreferredConceptName(String conceptName)
  {
      fPreferredConceptName = conceptName;
  }

  /**
   * set() method for disambiguation method weight.
   *
   * @param weight the weight assigned to the method.
   */
  public void setWweight(double weight)
  {
      fWeight = weight;
  }

  /**
   * get() method  for the disambiguation method name..
   *
   * @return the String representation of the method name.
   */
  public String getMethodName()
  {
      return fMethodName;
  }

  /**
   * get() method for the UMLS concept.
   *
   * @return the String representation of the concept name.
   */
  public String getPreferredConceptName()
  {
      return fPreferredConceptName;
  }

  /**
   * get() method for the method weight.
   *
   * @return the weight of the method.
   */
  public double getWeight()
  {
      return fWeight;
  }

  /**
   * Converts the ArbitratorRecord object information to String representation.
   * The format is <I>[method name|UMLS concept|weight]</I>
   *
   * @return the String representation of the ArbitratorRecord object.
   */
  public String toString()
  {
      return ("[" + fMethodName +  "|" + fPreferredConceptName + "|" + fWeight + "]").trim();
  }
}
