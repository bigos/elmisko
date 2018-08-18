class Population < ApplicationRecord
  has_many :results

  def samples
    res = results.take(3)
    { id: id, results: res }

    {"vals":{"stats":{"nominal":5.0,"mean":4.772857142857142,"deviation":0.15157003724508414},"qcresults":[{"id":31008274,"c":4.718,"d":"2018-04-12T13:34:51.000Z"},{"id":31008266,"c":4.71,"d":"2018-04-12T12:40:35.000Z"},{"id":31008260,"c":4.744,"d":"2018-04-12T12:23:19.000Z"},{"id":30984474,"c":4.552,"d":"2018-04-11T12:07:22.000Z"},{"id":30984465,"c":4.757,"d":"2018-04-11T11:04:41.000Z"},{"id":30953440,"c":4.903,"d":"2018-04-10T13:30:06.000Z"},{"id":30953428,"c":5.026,"d":"2018-04-10T12:28:07.000Z"}]}}

  end

  def create_sample_results
    ds = [0, 0, 1, 1, 2, 4, 6]
    ur = ds.length - 1
    (600).times do |x|
      dd = Time.now.at_midnight - x.days
      pp dd

      sidi = rand (0..ur)
      ss = ds[sidi]
      ss.times do |y|
        hr = rand (10..16)
        rt = dd + hr.hours
        nr = self.results.build
        nr.created_at = rt
        nr.updated_at = rt
        nr.concentration = rand (3.5 .. 7.4)
        nr.save
      end
    end
  end
end
