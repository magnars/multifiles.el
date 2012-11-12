Feature: Editing parts of multiple files in one buffer

  Background:
    Given I switch to buffer "*multifile*"
    And I press "<return>"
    And I open and erase file "/tmp/test1.txt"
    And I insert:
    """
    outside
    line a
    line b
    line c
    outside
    """
    And I go to the front of the word "line"
    And I set the mark
    And I go to the end of the word "c"
    And I press "C-f"
    And I press "C-!"

  Scenario: Opening multi-buffer from region
    When I switch to buffer "*multifile*"
    Then I should see:
    """
    line a
    line b
    line c
    """

  Scenario: Editing from multifile, center
    When I switch to buffer "*multifile*"
    And I go to the end of the word "line b"
    And I insert "ooya!"
    And I switch to buffer "test1.txt"
    Then I should see "booya!"

  Scenario: Editing from multifile, beginning
    When I switch to buffer "*multifile*"
    And I go to the front of the word "a"
    And I press "M-b"
    And I insert "sp"
    And I switch to buffer "test1.txt"
    Then I should see "spline a"

  Scenario: Editing from multifile, end
    When I switch to buffer "*multifile*"
    And I go to the end of the word "c"
    And I insert "ool"
    And I switch to buffer "test1.txt"
    Then I should see "cool"

  Scenario: Editing from multifile, outside top
    When I switch to buffer "*multifile*"
    And I go to the front of the word "a"
    And I press "M-b"
    And I press "C-b"
    And I insert "mirror-only"
    And I switch to buffer "test1.txt"
    Then I should not see "mirror-only"

  Scenario: Editing from multifile, outside bottom
    When I switch to buffer "*multifile*"
    And I go to the end of the word "c"
    And I press "C-f"
    And I insert "mirror-only"
    And I switch to buffer "test1.txt"
    Then I should not see "mirror-only"

  Scenario: Editing from original file
    When I switch to buffer "test1.txt"
    And I go to the end of the word "line b"
    And I insert "ooya!"
    And I switch to buffer "*multifile*"
    Then I should see "booya!"
