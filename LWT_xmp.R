library(exifr)
library(stringr)

#lf<-list.files("F:/CAMERA_TRAPPING/LWT camera trapping images/", recursive = TRUE, pattern = "*JPG", full.names = TRUE)

lf<-list.files("D:/Fiona/Biome_Health_Project/LWT_camera_trapping_images_tagged/Bardia_2017/Block_1/", recursive = TRUE, pattern = "*JPG", full.names = TRUE)

df<-read_exif(lf)
# 
tags<-df$Subject

spec_find<-c("Camera", "camera","Grid", "Location", "Blank", "Block",
            "NP", "Year", "Vehicle", "Human", "NIS", "Sex", "Set up",
            "Collection", "Fail", "Test", "Age", "Hunting")


xmp_sort<-function(file){

  cam<-file[grepl("Camera", file)]
  cam<-str_trim(gsub("Camera no:","", cam))

  grid<-file[grepl("Grid", file)]
  grid<-str_trim(gsub("Grid number:","", grid))

  loc<-file[grepl("Location", file)]
  loc<-str_trim(gsub("Location:","" ,file))

  human<-file[grepl("Human", file)]
  human<-paste(human, collapse = "; ")
  
  species<-file[!grepl(paste(spec_find, collapse = "|"), file)]
  
  return(species)
  
  }

spec_out<-lapply(tags, xmp_sort)

species<-unique(unlist(spec_out))

#write.csv(species, "species_lwt.csv", row.names = FALSE)

#write.csv(species, "species_lwt_update.csv", row.names = FALSE)

###


soi_df<-read.csv("species_lwt_EDIT.csv", stringsAsFactors = FALSE)
soi<-soi_df$x

xmp_sort_soi<-function(file){
  
  species<-file[grepl(paste(soi, collapse = "|"), file)]
  species<-species[!grepl("NIS", species)]
  
  if(length(species) > 0){
    species<-paste(species, collapse = "; ")
  }else{
    species<-NULL
    species<-NA
  }
  
  
  return(species)
  
}

spec_out<-lapply(tags, xmp_sort_soi)
species_id<-do.call("rbind",spec_out)

df_sp<-data.frame(df[,"SourceFile"], species_id)

multi_sp<-which(grepl(";",df_sp$species_id))


###Making the new folders

df_sp$folder_names<-as.character(df_sp$species_id)

df_sp$folder_names[multi_sp]<-"Multiple Species"


df_sub<-df_sp[!is.na(df_sp$species_id),]


d<-"M:/biome_health_project_files/country_files/nepal/lwt_photos/"

fn<-unique(na.omit(df_sub$folder_names))
fn<-gsub(" ", "_", fn)
fn<-gsub(":", "", fn)

dir_creator<-function(fn){

  if(!file.exists(paste0(d, fn))){
    dir.create(paste0(d, fn))  
    }
    
}

#lapply(fn, dir_creator)



###Making new file names



#df_sub$base_dirs<-gsub("F:/CAMERA_TRAPPING/LWT camera trapping images//Bardia 2017/","",df_sub$SourceFile)

df_sub$base_dirs<-gsub("D:/Fiona/Biome_Health_Project/LWT_camera_trapping_images_tagged/Bardia_2017/","",df_sub$SourceFile)

#df_sub$basefiles<-basename(df_sub$SourceFile)

df_sub$base_dirs<-gsub(" ", "_", df_sub$base_dirs)
df_sub$base_dirs<-gsub("/", "_", df_sub$base_dirs)
df_sub$base_dirs<-gsub("\\(", "", df_sub$base_dirs)
df_sub$base_dirs<-gsub(")", "", df_sub$base_dirs)


df_sub$folder_names<-gsub(" ", "_", df_sub$folder_names)
df_sub$folder_names<-gsub(":", "", df_sub$folder_names)


df_sub$new_filepaths<-paste(d, df_sub$folder_names,"/",df_sub$base_dirs, sep = "")



file.copy(df_sub$SourceFile, df_sub$new_filepaths)












