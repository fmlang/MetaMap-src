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

package wsd.methods;

import java.util.List;

import org.jdom.Document;

 /**
  * The public interface for all disambiguation methods. When a new disambiguation
  * method is added to the WSD system, it needs to implement getMatch()
  * method of this interface.
  *
  * <P>The XML structure a disambiguation method receives from the WSD Server
  * is expected to have the following format:
  *
  * <pre>
  * <b>  &lt;machine_output&gt;</b>
  * <b>      &lt;utterance ui</b>=".." <b>pos</b>=".." <b>sentence</b>="..."<b>&gt;</b>
  * <b>          &lt;phrase phrase_pos</b>=".." <b>noun_phrase</b>="..." <b>&gt;</b>
  * <b>              &lt;phrase_elements&gt;</b>
  * <b>                 &lt;phrase_element type</b>=".."<b>&gt;</b>
  * <b>                     &lt;field name</b>=".." <b>value</b>=".."<b>&gt;</b>
  * <b>                     &lt;field name</b>=".." <b>value</b>=".."<b>&gt;</b>
  * <b>                     &lt;field .... &gt;</b>
  * <b>                 &lt;/phrase_element&gt;</b>
  * <b>              &lt;/phrase_elements&gt;</b>
  * <b>              &lt;candidates&gt;</b>
  * <b>                 &lt;candidate score</b>=".." <b>cui</b>=".." <b>umls_concept</b>=".." <b>preferred_name</b>=".." <b>matched_words</b>=".." <b>semtypes</b>=".." <b>matchmap</b>=".." <b>head_flag</b>=".." <b>overmatch_flag</b>=".."<b>&gt;</b>
  * <b>                 &lt;candidate score</b>=".." <b>cui</b>=".." <b>umls_concept</b>=".." <b>preferred_name</b>=".." <b>matched_words</b>=".." <b>semtypes</b>=".." <b>matchmap</b>=".." <b>head_flag</b>=".." <b>overmatch_flag</b>=".."<b>&gt;</b>
  * <b>                 &lt;candidate ....&gt;</b>
  * <b>              &lt;/candidates&gt;</b>
  * <b>              &lt;mappings&gt;</b>
  * <b>                 &lt;mapping score</b>=".."<b>&gt;</b>
  * <b>                     &lt;candidate score</b>=".." <b>cui</b>=".." <b>umls_concept</b>=".." <b>preferred_name</b>=".." <b>matched_words</b>=".." <b>semtypes</b>=".." <b>matchmap</b>=".." <b>head_flag</b>=".." <b>overmatch_flag</b>=".."<b>&gt;</b>
  * <b>                     &lt;candidate score</b>=".." <b>cui</b>=".." <b>umls_concept</b>=".." <b>preferred_name</b>=".." <b>matched_words</b>=".." <b>semtypes</b>=".." <b>matchmap</b>=".." <b>head_flag</b>=".." <b>overmatch_flag</b>=".."<b>&gt;</b>
  * <b>                     &lt;candidate ....&gt;</b>
  * <b>                 &lt;/mapping&gt;</b>
  * <b>                 &lt;mapping score</b>=".."<b>&gt;</b>
  * <b>                     &lt;candidate score</b>=".." <b>cui</b>=".." <b>umls_concept</b>=".." <b>preferred_name</b>=".." <b>matched_words</b>=".." <b>semtypes</b>=".." <b>matchmap</b>=".." <b>head_flag</b>=".." <b>overmatch_flag</b>=".."<b>&gt;</b>
  * <b>                     &lt;candidate score</b>=".." <b>cui</b>=".." <b>umls_concept</b>=".." <b>preferred_name</b>=".." <b>matched_words</b>=".." <b>semtypes</b>=".." <b>matchmap</b>=".." <b>head_flag</b>=".." <b>overmatch_flag</b>=".."<b>&gt;</b>
  * <b>                     &lt;candidate ....&gt;</b>
  * <b>                 &lt;/mapping&gt;</b>
  * <b>                 &lt;mapping .... &gt;</b>
  * <b>              &lt;/mappings&gt;</b>
  * <b>              &lt;ambiguities&gt;</b>
  * <b>                 &lt;ambiguity process</b>="<i>yes/no</i>"<b>&gt;</b>
  * <b>                     &lt;candidate score</b>=".." <b>cui</b>=".." <b>umls_concept</b>=".." <b>preferred_name</b>=".." <b>matched_words</b>=".." <b>semtypes</b>=".." <b>matchmap</b>=".." <b>head_flag</b>=".." <b>overmatch_flag</b>=".."<b>&gt;</b>
  * <b>                     &lt;candidate score</b>=".." <b>cui</b>=".." <b>umls_concept</b>=".." <b>preferred_name</b>=".." <b>matched_words</b>=".." <b>semtypes</b>=".." <b>matchmap</b>=".." <b>head_flag</b>=".." <b>overmatch_flag</b>=".."<b>&gt;</b>
  * <b>                     &lt;candidate ....&gt;</b>
  * <b>                 &lt;/ambiguity&gt;</b>
  * <b>               &lt;/ambiguities&gt;</b>
  * <b>          &lt;/phrase&gt;</b>
  * <b>      &lt;/utterance&gt;</b>
  * <b>      &lt;methods&gt;</b>
  * <b>          &lt;method method_name</b>="..." <b>weight</b>=".."<b>&gt;</b>
  * <b>          &lt;method method_name ......  &gt;</b>
  * <b>       &lt;/methods&gt;</b>
  * <b>  &lt;/machine_output&gt;</b>
  * </pre>
  *
  * <P>This code was developed for National Library of Medicine, Cognitive
  * Science Branch.
  *
  * <p>Description: Word Sense Disambiguation</p>
  *
  * @version  04/02/02
  * @author   Halil Kilicoglu
  */
public interface DisambiguationMethod
{
   /**
     * getMatch() should be implemented by any disambiguation method.
     *
     * @param doc   a DOM tree that represents the MetaMap machine output that
     *              contains some ambiguity instances.
     *
     * @return      a List that contains the best matches found by this particular
     *              disambiguation method.
     *
     */
   List getMatch(Document doc);
}
