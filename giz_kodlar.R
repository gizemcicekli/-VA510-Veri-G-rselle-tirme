
install.packages("haven")
library(haven)
TÜM_VERİLER <- read_sav("TÜM VERİLER.sav")
View(TÜM_VERİLER)


str(TÜM_VERİLER)

install.packages("dplyr") # Veriyi daha rahat işlemek için
library(dplyr)

# Belirtilen değişkenleri faktörlere dönüştürme
veri_faktor1 <- TÜM_VERİLER %>%
  mutate(across(c(cinsiyet, medenidurum, egitimdurumu, calismadurumu, aylikgelir, yasamabicim, genelsaglik), as_factor))

str(veri_faktor1)

# Sütunları dönüştür Türkiye Sağlık Okuryazarlığı Ölçeği (TSOY-32) deki gibi olması için
veri_faktor2 <- mutate_at(veri_faktor1, vars(2:33), ~ case_when(
  . == 1 ~ 0,
  . == 2 ~ 1,
  . == 3 ~ 2,
  . == 4 ~ 3,
  . == 5 ~ 4,
  TRUE ~ NA_integer_
))


# Türkiye Sağlık Okuryazarlığı Ölçeği (TSOY-32) 2'den 32'ye kadar olan sütunlardaki verileri toplama ve yeni sütun ekleme


# 2-33. sütunlar arasındaki verileri her satır için ayrı ayrı toplayarak ortalamasını alma
ortalama <- rowMeans(veri_faktor2[, 2:33], na.rm = TRUE)

# İndeks puanını hesaplama
indeks <- (ortalama - 1) * (50/3)

# Sağlık okuryazarlığı düzeyini belirleme
düzey <- cut(indeks, breaks = c(0, 25, 33, 42, 50), labels = c("yetersiz", "sorunlu", "yeterli", "mükemmel"))

# Yeni sütunu veri çerçevesine ekleme
veri_faktor2$indeks <- indeks
veri_faktor2$düzey <- düzey

# Verinin ilk birkaç satırını görüntüleme
head(veri_faktor2)


#EKSIK DEĞER TESPİTİ VE DOLDURma

na_counts <- sapply(veri_faktor2, function(x) sum(is.na(x)))

na_counts

veri_faktor2$düzey[is.na(veri_faktor2$düzey)] <- "yetersiz"


veri_faktor2_df <- as.data.frame(veri_faktor2)


str(veri_faktor2_df)


###################################################3

# Gerekli paketleri yükle
library(ggplot2)

# Belirtilen mavi rengini tanımla
mavi <- "#1976D2"

# Düzey sütunundaki değerlerin sayısını ve yüzdesini hesapla
duzey_sayisi <- table(veri_faktor2_df$düzey)
duzey_yuzde <- prop.table(duzey_sayisi) * 100

# Çubuk grafik oluştur
ggplot(data = NULL, aes(x = reorder(names(duzey_sayisi), -duzey_sayisi), y = as.vector(duzey_sayisi))) +
  geom_bar(stat = "identity", fill = mavi) +
  geom_text(aes(label = paste0(duzey_sayisi, " (", round(duzey_yuzde, 1), "%)")), vjust = -0.5, size = 3, color = "black") +
  labs(x = "Sağlık Okuryazarlığı Düzeyi", y = "Kişi Sayısı", title = "Sağlık Okuryazarlığı Düzeyinin Dağılımı") +
  theme_linedraw(base_size = 18) + # Genel yazı boyutunu artır
  theme(
    plot.title = element_text(size = 18, face = "bold"), # Başlık boyutu ve kalınlığı
    axis.title.x = element_text(size = 14, face = "bold"), # X ekseni başlık boyutu ve kalınlığı
    axis.title.y = element_text(size = 14, face = "bold"), # Y ekseni başlık boyutu ve kalınlığı
    axis.text = element_text(size = 12), # Eksen üzerindeki yazılar
    legend.title = element_text(size = 14, face = "bold"), # Lejand başlığı
    legend.text = element_text(size = 10) # Lejand içeriği
  )


##################################################################################################


# Yığılmış çubuk grafik için veriyi hazırlama
veri_faktor2_df %>% 
  count(cinsiyet, düzey) %>% 
  group_by(cinsiyet) %>% 
  mutate(pct = n / sum(n) * 100) -> veri_summary

# Yığılmış çubuk grafik oluşturma
ggplot(veri_summary, aes(y = cinsiyet, x = pct, fill = düzey)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Cinsiyet ile Sağlık Okuryazarlığı İlişkisi",
       x = "Yüzde",
       y = "Cinsiyet",
       fill = "Düzey") +
  theme_linedraw(base_size = 18)


####################################################################################################################
################################################################################
##################################################################################################
##################################################################################################


# Kadın ve erkek sayısını öğrenmek için cinsiyet sütunundaki değerlerin sayısını hesapla
cinsiyet_sayisi <- table(veri_faktor2_df$cinsiyet)

# Sonuçları yazdır
print(cinsiyet_sayisi)


################################################################################
##################################################################################################
##################################################################################################


# Gerekli paketleri yükle
library(ggplot2)

# Kadın ve erkek sayısını öğrenmek için cinsiyet sütunundaki değerlerin sayısını hesapla
cinsiyet_sayisi <- table(veri_faktor2_df$cinsiyet)
cinsiyet_yuzde <- prop.table(cinsiyet_sayisi) * 100

# Renk paletini tanımla
vivid_palette <- c("#D32F2F", "#1976D2") # Koyu kırmızı ve mavi renkler

# Kadın ve erkek sayısını donut grafiğinde göster
ggplot(data = as.data.frame(cinsiyet_sayisi), aes(x = 2, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1, color = "white") +  # Her bir dilimin genişliğini ayarla ve beyaz renkte kenarlıklar ekle
  geom_text(aes(label = paste0(round(cinsiyet_yuzde, 1), "%\n ", Freq, " kişi")), 
            position = position_stack(vjust = 0.5), size = 4, color = "black") + # Etiketler
  coord_polar(theta = "y") +
  scale_fill_manual(values = vivid_palette) +
  labs(x = NULL, y = NULL, fill = "Cinsiyet", title = "Cinsiyet Dağılımı ") +
  theme_void(base_size = 18) +  # Boş tema
  xlim(0.5, 2.5)  # Boş bir orta kısım oluşturmak için x limiti ayarla



########BASARILI YASLANMA#######


# basyas1'den basyas14'e kadar olan sütunların toplamını basyaspuan adlı yeni bir sütuna ekle
veri_faktor2_df <- veri_faktor2_df %>%
  mutate(basyaspuan = rowSums(select(., basyas1:basyas14), na.rm = TRUE))

# Yeni veriyi kontrol et
head(veri_faktor2_df)


################################################################################

# Cinsiyete göre basyaspuan ortalamasını hesaplama
ortalama_basyaspuan <- veri_faktor2_df %>%
  group_by(cinsiyet) %>%
  summarise(ortalama = mean(basyaspuan, na.rm = TRUE))

# Çubuk grafik çiz
ggplot(ortalama_basyaspuan, aes(x = cinsiyet, y = ortalama, fill = cinsiyet)) +
  geom_bar(stat = "identity", width = 0.5, color = "black") +
  geom_text(aes(label = round(ortalama, 2)), vjust = -0.5, size = 3, color = "black") +
  labs(x = "Cinsiyet", y = "Başarılı Yaşlanma Puanı", title = "Cinsiyete Göre Başarılı Yaşlanma Puan Ortalaması") +
  scale_fill_manual(values = c("erkek" = "#1976D2", "kadin" = "#D32F2F")) + # Erkek için mavi, Kadın için kırmızı
  theme_linedraw(base_size = 18)+
  theme(legend.position = "none") # Lejantı kaldır


################################################################################
# BASARILI YASLANMA PUANI İLE İNDEKS ARASINDAKİ İLİŞKİ

# Scatter plot (Dağılım grafiği) oluştur
ggplot(veri_faktor2_df, aes(x = basyaspuan, y = indeks, color = cinsiyet)) +
  geom_point(alpha = 0.6, position = position_jitter(width = 0.2, height = 0.2)) +
  scale_color_manual(values = c("kadin" = "darkred", "erkek" = "blue")) +
  labs(x = "Başarılı Yaşlanma Puanı", y = "Sağlık Okuryazarlığı İndeksi", title = "Başarılı Yaşlanma Puanı ve Okuryazarlık İndeksi Arasındaki İlişki") +
  theme_linedraw(base_size = 18) +
  theme(legend.title = element_blank())


################################################################################
# BASARILI YASLANMA PUANI İLE Eğitim durumu 

# Boxplot oluştur
ggplot(veri_faktor2_df, aes(x =as.factor(egitimdurumu), y = basyaspuan, fill = as.factor(egitimdurumu))) +
  
  geom_boxplot() +
  scale_fill_brewer(palette = "Set3") +
  
  labs(x = "Eğitim Durumu", y = "Başarılı Yaşlanma Puanı", title = "Eğitim Durumu ve Başarılı Yaşlanma Puanı Dağılımı") +
  theme_linedraw(base_size = 12) +
  theme(
    plot.title = element_text(size = 18, face = "bold"), # Başlık boyutu ve kalınlığı
    axis.title.x = element_text(size = 14, face = "bold"), # X ekseni başlık boyutu ve kalınlığı
    axis.title.y = element_text(size = 14, face = "bold"), # Y ekseni başlık boyutu ve kalınlığı
    axis.text = element_text(size = 18) # Eksen üzerindeki yazılar
  )+
  theme(legend.position = "none")  # Lejantı gizle





################################################################################
#(POSTERDE KULLNMADIM)

# BASARILI YASLANMA PUANI İLE GENEL SAĞLIK DURUMU

# Boxplot oluştur
ggplot(veri_faktor2_df, aes(x = as.factor(genelsaglik), y = basyaspuan, fill = as.factor(genelsaglik))) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set3") +
  labs(x = "Genel Sağlık Durumu", y = "Başarılı Yaşlanma Puanı", title = "Başarılı Yaşlanma ve Genel Sağlık Durumu") +
  theme_linedraw(base_size = 12)+
  theme(legend.position = "none")  # Lejantı gizle

################################################################################
# BASARILI YASLANMA PUANInın Sğlık okuryazarlığı ve eğitim düzeyine göre dağılımı
#(POSTERDE KULLNMADIM)

# Yumuşak ve parlak olmayan bir renk paleti tanımla
soft_palette <- c("#4CAF50", "#FFC107", "#2196F3", "#9C27B0", "#FF5722") 

# Eğitim durumuna göre gruplandırılmış basyaspuan ortalamalarını hesapla
basyaspuan_ortalamalari <- veri_faktor2_df %>%
  group_by(düzey, egitimdurumu) %>%
  summarise(ortalama_basyaspuan = mean(basyaspuan, na.rm = TRUE))

# Çoklu çubuk grafiği oluştur
ggplot(basyaspuan_ortalamalari, aes(x = düzey, y = ortalama_basyaspuan, fill = egitimdurumu)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Sağlık Okuryazarlığı Düzeyi", y = "Başarılı Yaşlanma Puanı", fill = "Eğitim Durumu", title = "Başarılı Yaşlanamaya Göre Okuryazarlık ve Eğitim") +
  scale_fill_manual(values = soft_palette) +  # Yumuşak paleti kullan
  theme_linedraw(base_size = 12) +
  theme(legend.position = "right")  # Lejantı sağ üst köşeye taşı

################################################################################

# #(POSTERDE KULLNMADIM)
# Düzeye göre basyaspuan ortalamasını hesapla
ortalama_basyaspuan_duzey <- veri_faktor2_df %>%
  group_by(düzey) %>%
  summarise(ortalama = mean(basyaspuan, na.rm = TRUE))

# Çubuk grafik çiz
ggplot(ortalama_basyaspuan_duzey, aes(x = düzey, y = ortalama, fill = düzey)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_text(aes(label = round(ortalama, 2)), vjust = -0.5, size = 3, color = "black") +
  labs(x = "Sağlık Okuryazarlığı Düzeyi", y = "Başarılı Yaşlanma Puanı", title = "Okuryazarlık Düzeyine Göre Başarılı Yaşlanma Puanı Ortalaması") +
  scale_fill_brewer(palette = "Set3") + # Renk paleti
  theme_linedraw(base_size = 12) +
  theme(legend.position = "none") # Lejantı kaldır



##################################################################################################
#(POSTERDE KULLNMADIM)

library(ggplot2)

# Kernel Yoğunluk Diyagramı
ggplot(veri_faktor2_df, aes(x = yas, y = indeks, color = cinsiyet)) +
  geom_jitter(alpha = 0.5, width = 0.3, height = 0.3) +  # Noktaları yayarak yoğunluk gösterimi
  geom_density_2d(aes(color = cinsiyet), alpha = 0.7) +  # Kontur çizgileri
  scale_color_manual(values = c("erkek" = "blue", "kadin" = "darkred")) +
  labs(title = "Cinsiyete Göre Yaş ve İndeks Dağılımı",
       x = "Yaş",
       y = "İndeks") +
  theme_linedraw(base_size = 14)

##################################################################################################

#PASTA GRAFİĞİ (POSTERDE KULLNMADIM)
# Doygun renk paletini tanımla
vivid_palette <- c("#D32F2F", "#388E3C", "#7B1FA2", "#1976D2") # Doygun kırmızı, yeşil, mor ve mavi renkler

# Pasta grafiği oluştur ve yüzde ve sayıları ekleyerek
ggplot(data = NULL, aes(x = "", y = as.vector(duzey_sayisi), fill = reorder(names(duzey_sayisi), -duzey_sayisi))) +
  geom_bar(stat = "identity", width = 5, color = "white") +  # Her bir dilimin genişliğini 5 olarak ayarla ve beyaz renkte kenarlıklar ekle
  geom_text(aes(label = paste0(round(duzey_yuzde, 1), "%\n(", duzey_sayisi, " kişi)")), position = position_stack(vjust = 0.5), size = 4, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = vivid_palette) +
  labs(x = NULL, y = NULL, fill = "Düzey", title = "Düzey Sütununa Göre Kişi Sayısı") +
  theme_void()
