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
 * This class represents a a noun phrase in an utterance, its elements and
 * candidates, mappings, ambiguities in that phrase.
 *
 * <P>This code was developed for National Library of Medicine, Cognitive
 * Science Branch.
 *
 * <p>Description: Word Sense Disambiguation</p>
 * <p>Copyright: Copyright (c) 2002</p>
 * <p>Company: Aquilent, Inc. </p>
 *
 * @version  04/02/02
 * @author   Halil Kilicoglu
 */

public class NounPhrase
{
  /** noun phrase position in the utterance */
  private int fPos;
  /** the noun phrase text */
  private String fPhrase;
  /** the list of noun phrase elements (head, mod, etc.) */
  private List fPhraseElements;
  /** the list of candidate concepts for the phrase */
  private List fCandidates;
  /** the list of mappings for the phrase */
  private List fMappings;
  /** the ambiguities in the phrase */
  private List fAmbiguities;

  /**
   * Default constructor for the class. No parameters.
   */
  public NounPhrase()
  {
  }

  /**
   * Constructor. Sets the member fields when the object is created.
   *
   * @param pos           noun phrase position in the utterance
   * @param phrase        noun phrase string
   * @param elements      noun phrase elements
   * @param candidates    candidate concepts
   * @param mappings      mappings found for the phrase
   * @param ambiguities   a list of ambiguities in the phrase, if any.
   */
  public NounPhrase(int pos,
                   String phrase,
                   List elements,
                   List candidates,
                   List mappings,
                   List ambiguities)
  {
      fPos = pos;
      fPhrase = phrase;
      fPhraseElements = elements;
      fCandidates = candidates;
      fMappings = mappings;
      fAmbiguities = ambiguities;
  }

  /**
   * Constructor. Creates a NounPhrase object from an XML tree node. Element,
   * candidate, mapping and ambiguity lists are created empty. set() methods
   * should be used to set them later.
   *
   * @param node  noun phrase node in the XML tree.
   * @param ns    default namespace.
   *
   */
  public NounPhrase(Element node, Namespace ns)
  {
      fPos = Integer.parseInt(node.getAttributeValue("pos",ns));
      fPhrase = node.getAttributeValue("noun_phrase",ns);
      fPhraseElements = new Vector();
      fCandidates = new Vector();
      fMappings = new Vector();
      fAmbiguities = new Vector();
  }

  /**
   * set() method for noun phrase position.
   *
   * @param  pos    noun phrase position
   */
  public void setPos(int pos)
  {
      fPos = pos;
  }

  /**
   * set() method for noun phrase.
   *
   * @param  phrase    the noun phrase
   */
  public void setPhrase(String phrase)
  {
      fPhrase = phrase;
  }

  /**
   * set() method for phrase elements. Each phrase element consists of a key-value
   * pairs. Key is the name of the element, and value is its value.
   *
   * @param   elements    the list of phrase elements
   */
  public void setPhraseElements(List elements)
  {
      fPhraseElements = elements;
  }

  /**
   * set() method for the candidate list that consists of Candidate objects.
   *
   * @param   candidates    the list of candidates
   */
  public void setCandidates(List candidates)
  {
      fCandidates = candidates;
  }

  /**
   * set() method for the mappings list.
   *
   * @param   mappings    the list of mappings
   */
  public void setMappings(List mappings)
  {
      fMappings = mappings;
  }

  /**
   * set() method for ambiguities.
   *
   * @param   ambiguities    the ambiguities vector
   */
  public void setAmbiguities(List ambiguities)
  {
      fAmbiguities = ambiguities;
  }

   /**
   * get() method for noun phrase position.
   *
   * @return  noun phrase position
   */
  public int getPos()
  {
      return fPos;
  }

  /**
   * get() method for noun phrase.
   *
   * @return  noun phrase
   */
  public String getPhrase()
  {
      return fPhrase;
  }

 /**
   * get() method for the phrase elements.
   *
   * @return the phrase element vector
   */
  public List getPhraseElements()
  {
      return fPhraseElements;
  }

 /**
   * get() method for the candidates.
   *
   * @return the candidate vector
   */
  public List getCandidates()
  {
      return fCandidates;
  }

 /**
   * get() method for the mappings.
   *
   * @return the mappings vector
   */
  public List getMappings()
  {
      return fMappings;
  }

  /**
   * get() method for ambiguities.
   *
   * @return the ambiguities vector
   */
  public List getAmbiguities()
  {
      return fAmbiguities;
  }

  /**
   * creates an easy-to-read string representation of the noun phrase.
   * Used mostly for debugging purposes. The representation has the following format:
   * <P> <i>[Noun phrase position|Noun phrase]</i>"
   *
   * @return  the string representation of the noun phrase.
   */
  public String toString()
  {
      return ("[" + fPos + "|" +  fPhrase + "]").trim();
  }
}
