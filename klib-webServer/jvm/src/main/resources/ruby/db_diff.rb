require 'sqlite3'

def log(label, color, message)
  puts("[\e[#{color}m#{label}\e[39m]: #{message.to_s.gsub("\n", "\n       : ")}")
end

db_name, *schemas = ARGV

db_path = db_name
log("INFO ", 32, "db_path: #{db_path}")

db = SQLite3::Database.new(db_path)

rows = db.execute <<-SQL
SELECT 
  m.name as t_name,
  p.name as c_name,
  p.type as c_type,
  p."notnull" as c_notnull
FROM sqlite_master m
  left outer join pragma_table_info((m.name)) p
    on m.name <> p.name
  where m.type = 'table';
SQL

db_table_map =
  rows
    .filter { |r| r[0] != 'sqlite_sequence' }
    .group_by { |r| r[0] }
    .transform_values { |vs| vs.map { |v| [v[1], { c_name: v[1], c_type: v[2], c_notnull: v[3] > 0 }] }.to_h }

rows = db.execute <<-SQL
SELECT 
  m.name as t_name
FROM sqlite_master m
  where m.type = 'index';
SQL

db_index_list = rows.map { |r| r[0] }

schemas_mapped = schemas.map do |schema|
  [
    schema,
    if schema.start_with?("create table")
      head, *mid, _ = schema.split("\n")

      table_name = /^create table (\S*) \($/.match(head)[1]
      cols = mid.map do |m|
        line = m.strip.chomp(",")
        res = /^(\S*) (\S*)( not null)?.*$/.match(line)
        { c_name: res[1], c_type: res[2], c_notnull: !res[3].nil?, sql: line }
      end

      {
        type: :table,
        data: {
          table_name: table_name,
          cols: cols,
        },
      }
    elsif schema.start_with?("create unique index")
      res = /^create unique index (idx\S*).*$/.match(schema)[1]

      {
        type: :index,
        data: res,
      }
    else
      nil
    end
  ]
end

unmatched_schemas = schemas_mapped.filter_map { |d| d[1].nil? ? d[0] : nil }

if unmatched_schemas.any?
  puts("[\e[31mERROR\e[39m]: UNMATCHED SCHEMAS\e[39m")
  unmatched_schemas.each { |s| puts("[\e[31mERROR\e[39m]: #{s.gsub("\n", "\n       : ")}") }
  exit(1)
end

errors = false

def log_and_execute(db, sql)
  log("INFO ", 32, "Executing sql:\n#{sql}")
  db.execute(sql)
end

schemas_mapped.each do |t|
  begin
    schema, d = t
    type = d[:type]
    data = d[:data]

    case type
    when :table
      table_name = data[:table_name]
      cols = data[:cols]

      db_table_cols = db_table_map[table_name]
      if db_table_cols.nil?
        log_and_execute(db, schema)
      else
        cols.each do |col|
          db_col = db_table_cols[col[:c_name]]
          if db_col.nil?
            log_and_execute(db, "ALTER TABLE #{table_name} ADD COLUMN #{col[:sql]};")
          end
        end
      end
    when :index
      unless db_index_list.include?(data)
        log_and_execute(db, schema)
      end
    else
      raise "WTF..."
    end
  rescue => e
    log("ERROR", 31, e.message)
    errors = true
  end
end

exit(errors ? 1 : 0)

# TODO : Drop tables/indices/columns that no longer exist, also do more diffs on columns
