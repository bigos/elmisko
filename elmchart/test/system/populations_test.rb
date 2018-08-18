require "application_system_test_case"

class PopulationsTest < ApplicationSystemTestCase
  setup do
    @population = populations(:one)
  end

  test "visiting the index" do
    visit populations_url
    assert_selector "h1", text: "Populations"
  end

  test "creating a Population" do
    visit populations_url
    click_on "New Population"

    fill_in "Name", with: @population.name
    click_on "Create Population"

    assert_text "Population was successfully created"
    click_on "Back"
  end

  test "updating a Population" do
    visit populations_url
    click_on "Edit", match: :first

    fill_in "Name", with: @population.name
    click_on "Update Population"

    assert_text "Population was successfully updated"
    click_on "Back"
  end

  test "destroying a Population" do
    visit populations_url
    page.accept_confirm do
      click_on "Destroy", match: :first
    end

    assert_text "Population was successfully destroyed"
  end
end
