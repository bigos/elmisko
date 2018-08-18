class CreateResults < ActiveRecord::Migration[5.2]
  def change
    create_table :results do |t|
      t.integer :population_id
      t.float :concentration

      t.timestamps
    end
  end
end
