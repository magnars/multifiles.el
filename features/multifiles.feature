Feature: Editing parts of multiple files in one buffer

  Scenario: Opening multi-buffer from region
    Given I open and erase file "/tmp/test1.txt"
    And I insert:
    """
    outside
    line a
    line b
    line c
    outside
    """
    When I go to the front of the word "line a"
    And I set the mark
    And I go to the end of the word "line c"
    And I press "C-!"
    And I switch to buffer "*multifile*"
    Then I should see "/tmp/test1.txt"
    And I should see:
    """
    line a
    line b
    line c
    """
